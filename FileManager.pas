unit FileManager;

{
  Сохранение измененных файлов можно сделать только с помощью функции SaveAsDir

TODO:
  Для кеша не подходит использование TDictionary - требуется переделать на другой класс
}

interface

uses SysUtils, Windows, Classes, System.Generics.Collections, System.Generics.Defaults,
  Types, StrUtils, DirectoryManager, System.Threading, RecordUtils;

type
  TCacheOperation = (
      coDropNotChanged,   //из кеша удаляются только объекты которые не менялись
      coDropChanges,      //из кеша удаляются все объекты
      coSaveChanges       //при удалении из кеша изменненые объекты будут записаны на свои реальные места
      //данный вариант использовать осторожно, желательно только в редакторе, т.к. может перезаписать реальные файлы или данные в аддоне
    );
  TFileTypes = (
      ftAllInFolder,      //папки и файлы в указанной папке
      ftFiles,            //файлы в указанной папке
      ftFolders,          //папки в указанной папке
      ftFilesInSubfolders //файлы в указанной папке и её подпапках
    );
  {
    При сохранении к работе с потоком предъявляются следующие требования:
      - по окончании вызова поток должен находится в позиции для записи новых байт
      - в поток может производится только запись
      - движения/поиск по потоку назад/вперед запрещены
    Гарантии:
      - размер потока возвращает размер реально записанных данных этой функцией (без учета сжатия)
  }
  TStreamOptions = set of (
      soNoCompression, //запись в поток будет производиться без сжатия
      soNeedReset, // доступна команда Position:= 0
      soNeedSize, // доступно получение размера потока
      soNeedSeekForward, // доступно перемещение вперёд по потоку
      soNeedSeekBackward, // доступно перемещение назад по потоку
      ///  soNeedReset, soNeedSeekForward, soNeedSeekBackward вместе должны гарантировать перемещение в абсолютную позицию
      soNeedDeffered, //уничтожением потока управляет сам объект
      ///  возвращает THandleStream
      ///  если soNeedDeffered, то должен быть не связан с потоком всего архива
      ///  soNeedReset, soNeedSize вынуждают создавать копию файла (чтобы размер и позиция совпадали с физическими)
      ///  если сочетание флагов (soNeedHandle, soNeedDeffered, soNeedReset, soNeedSize) не поддерживается аддоном,
      ///  то должен вернуть любой поток с поддержкой soNeedSize, менеджером будет автоматически созданвременный файл
      soNeedHandle
      );

  {
    При изменении объект должен менять свою метку на текущую, получая ее у менеджера ресурсов
  }
  TChangeStamp = type LongWord;//NativeInt

const
  csAllChanges = 0;
  csIgnoreChanges = TChangeStamp(-1);

  AddOnSign = 'GAO';
  MinBlockSize = 128;

type
  IFileObject = interface
    function GetObject: TObject;
    function GetChangeStamp: TChangeStamp;
    procedure SaveToStream(AStream: TStream);
    procedure ReloadObject(AStream: TStream);
    procedure ResetChangeStamp;
  end;

  //использеутся для делегирования интерфейса IFileObject в безинтерфейсный тип
  TFileObjectDelegate = class (TInterfacedObject)
  private
    FOwner: TObject;
    FChanged: TChangeStamp;
  protected
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
    function GetObject: TObject;
    function GetChangeStamp: TChangeStamp;
    procedure SetChangeStamp;
    procedure ResetChangeStamp;
  end;

  //используется для делегирования интерфейса IFileObject в тип с интерфейсом
  TFileObjectAggregated = class (TAggregatedObject)
  private
    FOwner: TObject;
    FChanged: TChangeStamp;
  protected
  public
    constructor Create(AOwner: TObject; const Controller: IInterface);
    function GetObject: TObject;
    function GetChangeStamp: TChangeStamp;
    procedure SetChangeStamp;
    procedure ResetChangeStamp;
  end;

  TSimpleFileObject = class (TInterfacedObject, IFileObject)
  private
    FChanged: TChangeStamp;
  public
    function GetObject: TObject;
    function GetChangeStamp: TChangeStamp;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure ReloadObject(AStream: TStream); virtual; abstract;
    procedure ResetChangeStamp;
  end;

  TAddOnFileInfo = packed record  //размер 32 байта
    RealSize: UInt64;      //размер несжатого файла
    BlockCount: LongWord;  //кол-во блоков под файл (-1 так как меньше одного не может быть)
    BlockNumber: LongWord; //указатель на первый блок
    NameNumber: UInt64;
    CompressionAlg: Word;
    Flags: Word;
    FileCRC32: LongWord;  //контрольная сумма всех блоков файла, лишние байты считаются нулевыми
  end;

  //с этого может начинаться блок
  TAddOnFileTable = packed record  //заголовок размером 32 байта
    NextTableBlockNum: LongWord; //0 - следующего блока нет
    FileCountInCurrentTable: LongWord;
    NameBlocks: array [0..3] of LongWord;
    HashBlocks: array [0..1] of LongWord; //дополнительные хеши
  end;
  PAddOnFileTable = ^TAddOnFileTable;

  //с этого может начинаться блок
  TAddOnHeader = packed record     //основная часть размером 32 байта
    Sign: array [0..3] of AnsiChar;
    FileVersion: Word;
    TableCount: Word;      //кол-во таблиц размещения, помимо первой, которая в заголовке
    BlockSize: LongWord;   //размер блока деленный на 128, т.к. это минимальный размер
    BlockCount: LongWord;  // = (реальное кол-во - 1), т.к. первый блок включает в себя заголовок
    //указатель на блоки с хешами, если хеш попадает на текущий блок с хешами,
    //он считается после заполнения всех остальных хешей в этом блоке, считается, что текущий хеш нулевой
    HashBlocks: array [0..3] of LongWord;
  end;
  PAddOnHeader = ^TAddOnHeader;

  TAddOnFullHeader = packed record
    Header: TAddOnHeader;
    FirstFileTable: TAddOnFileTable;
    Files: array [0..0] of TAddOnFileInfo;
  end;
  PAddOnFullHeader = ^TAddOnFullHeader;

  //минимальный размер файла - 3 блока (1 - заголовок, 2 - имена, 3 - содержимое одного файла) - 128*3

  IAddOn = interface
    function FileExist(const FileName: string): Boolean;
    function GetFile(const FileName: string; Options: TStreamOptions): TStream;
    procedure GetFileList(List: TStrings);
  end;

  IWritableAddOn = interface
    function CreateNewFile(const FileName: string; FileSize: Integer = 0): TStream;
    procedure SaveToStream(Stream: TStream);
  end;

  TAddOn = class (TInterfacedObject, IAddOn, IWritableAddOn)
  private
    FStream: TStream;
    FFileNames: TStringList;
    FHeader: PAddOnFullHeader;
    FFileTables: array of TAddOnFileTable;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenFromFile(const FileName: string; Mode: Word);
    procedure LoadFromStream(Stream: TStream);
    //поток содержит только этот аддон, временем жизни потока управляет этот аддон
    procedure OpenFromOwnedStream(Stream: TStream);
    function CreateNewFile(const FileName: string; FileSize: Integer = 0): TStream;
    function OpenFile(const FileName: string): TStream;
    procedure SaveToStream(Stream: TStream);
    procedure AddNewFile(const Name: string; Obj: IFileObject);
    function FileExist(const FileName: string): Boolean;
    function GetFile(const FileName: string; Options: TStreamOptions): TStream;
    procedure GetFileList(List: TStrings);
  end;

  TDirectoryCache = class;

  TFileManager = class;

  TCacheInstance = class
  public
    Next: TCacheInstance;
    DirectoryCache: TDirectoryCache;
    constructor Create(ANext: TCacheInstance; ADirectoryCache: TDirectoryCache);
  end;

  TCreateStream = function : TStream of object;

  IFutureWithSoftCancel = interface (IFuture<IFileObject>)
    ['{3389434A-32C6-4C19-AAA4-7CF4A8C93F25}']
    procedure SoftCancel;
    function IsSoftCanceled: Boolean;
  end;

  TCacheData = class;

  TThreadPoolHelper = class helper for TThreadPool
    class function GetObjectCachesPublic: TObjectCaches;
  end;

  TFileObjectLoader = class sealed(TTask, IFutureWithSoftCancel)
  private
    FResult: IFileObject;
    FSoftCanceled: Boolean;
    class constructor Create;
    procedure RunEvent(Sender: TObject);
  protected
    function Start: IFuture<IFileObject>;
    function GetValue: IFileObject;
    procedure SoftCancel;
    function IsSoftCanceled: Boolean;

    constructor Create(Sender: TCacheData; APool: TThreadPool);
  public
    class function CurrentTask: IFutureWithSoftCancel; static; inline;
    class function Future(AFileData: TCacheData; APool: TThreadPool = nil): IFutureWithSoftCancel; overload; static; inline;
    class function TrySync(AFileData: TCacheData; APool: TThreadPool = nil): IFutureWithSoftCancel; static; inline;
  end;

  TFileObjectCreator = function (Self: Pointer; Manager: TFileManager; const RealPath: string): IFileObject; register;
  TFileObjectCreatorObj = function (Manager: TFileManager; const RealPath: string): IFileObject of object; register;

  TCacheData = class (TInterfacedObject)
  strict private
    FTask: IFutureWithSoftCancel;
    FManager: TFileManager;
    Instance: TCacheInstance; //содержит все ссылки на данную запись
  private
    FFileObject: IFileObject;
  public
    AddOn: IAddOn;
    RealPath: string; //для аддонов содержит название в рамках аддона
    IsSaved: Boolean;
    Options: TStreamOptions;
    function IsCreated: Boolean; inline;
    constructor Create(AManager: TFileManager);
    function CreateStream: TStream;
    function LoadAsync(ACreator: TFileObjectCreatorObj): IFutureWithSoftCancel;
    function TryLoadSync(ACreator: TFileObjectCreatorObj): IFutureWithSoftCancel;
    function Load: IFileObject;
    procedure ClearCache(Initiator: TDirectoryCache; const Name: string);
    procedure AddDirectory(Dir: TDirectoryCache);
  end;

  TMaskLoader = record
    FileExt: string;
    LoaderFunction: TFileObjectCreatorObj;
    StreamOptions: TStreamOptions;
  end;

  TLoadedAddOn = record
    Root: string;
    AddOn: IAddOn;
  end;

  TFolderConnect = record
    Root: string;    //виртуальный
    Folder: string;  //реальный путь
  end;

  TSaveOptions = class
  private
  protected
  public
  end;

  TAddOnFile = record
    FileName: string;
    AddOn: IAddOn;
  end;
  PAddOnFile = ^TAddOnFile;

  TDirectoryComparer = class (TInterfacedObject, IComparer<TPair<string, TObject>>)
  public
    function Compare(const Left, Right: TPair<string, TObject>): Integer;
  end;

  TDirectoryCache = class
  private class var
    FDirectoryComparer: IComparer<TPair<string, TObject>>;
  public
    class property DirectoryComparer: IComparer<TPair<string, TObject>> read FDirectoryComparer;
    class constructor Create;
    class destructor Destroy;
  private
    FData: TListRecord<TPair<string, TObject>>;
    FPath: string;
    function GetDirectory(const Name: string): TDirectoryCache; overload;
    function GetDirectory(const List: array of string): TDirectoryCache; overload;
    function GetFileCache(const Name: string): TCacheData;
    procedure SetDirectory(const Name: string; const Value: TDirectoryCache);
    procedure SetFileCache(const Name: string; const Value: TCacheData);
    function GetCount: Integer;
  strict protected
    function AddObject(const S: string; Value: TObject): Integer;
  protected
    function GetItem(Index: Integer): string; inline;
    function GetObject(Index: Integer): TObject; inline;
    function Find(const S: string; var Index: Integer): Boolean;
  public
    procedure RecursiveDropNotChanged;
    constructor Create(const APath: string);
    property Directory[const Name: string]: TDirectoryCache read GetDirectory;
    property FileCache[const Name: string]: TCacheData read GetFileCache write SetFileCache;
    property Path: string read FPath;
    property Items[Index: Integer]: string read GetItem; default;
    property Objects[Index: Integer]: TObject read GetObject;
    property Count: Integer read GetCount;
    function TryFileCache(const Name: string; var Cache: TCacheData): Boolean;
    procedure AddCache(const Name: string; Cache: TCacheData);
    procedure Clear(DoSave: Boolean);
    destructor Destroy; override;
  end;

  {Принцип работы:
   при запросе файла  происходит поиск в кеше (поиск среди проинициализированных), если в кеше есть запись, то выдается она,
   далее ищется файл в файловой системе, если находится, то создается запись в кеше (помечается как ФС),
   иначе ищется в текущих загруженных аддонах

   текущие загруженные аддоны так же являются кешем, по этой причине если дело дошло до поиска файла в
   файловой системе, то если запись есть в кеше, но не была найдена в ФС, она помечается как отсутствующая
   в ФС и больше не ищется там при следующих обращениях, пока кеш не очистят.
  }
  TFileManager = class
  private
    FRootDirectory: string;
    FResourceLoaders: TList<TMaskLoader>;
    FLoadedAddOns: TList<TLoadedAddOn>;
    FFolderConnects: TList<TFolderConnect>;
    FOnLoaderNotFound: TFileObjectCreatorObj;
    FCache: TDirectoryCache;
    FCurrentStamp: TChangeStamp;
    FNextRound: Boolean;
  protected
    property LoadedAddOns: TList<TLoadedAddOn> read FLoadedAddOns;
    property FolderConnects: TList<TFolderConnect> read FFolderConnects;
    procedure SetRootDirectory(const ARootDirectory: string);
    function GetLoaderByName(const FileName: string; var Options: TStreamOptions): TFileObjectCreatorObj;
    procedure DisconnectDirectory(Index: Integer); overload;
    procedure OnCacheNotify(Sender: TObject; const Item: TCacheData; Action: TCollectionNotification);
    //данные функции принимают уже обработанные строки (в нижнем регистре, с развернутыми путями)
    {
      Добавляет запись в кеш, гарантируя целостность относительных ссылок
      RealFile - наименование реального файла
      RelativePath - наименование виртуального файла
      если файл реальный, то эти переменные совпадают
    }
    function AddCacheNote(const RealFile, RelativePath: string; AddOn: PAddonFile; IsVirtual: Boolean = False): TCacheData;
    function TryCacheNote(const RelativePath: string; CreateNew: Boolean = False): TCacheData;
    function GetFileStream(Cache: TCacheData; Options: TStreamOptions): TStream; overload;
    function DoGetObject(const FileName: string; Func: TFileObjectCreatorObj; Options: TStreamOptions; TrySync: Boolean): IFutureWithSoftCancel;
  public
    procedure LogAll;
    constructor Create(const ARootDirectory: string = '');
    destructor Destroy; override;
    property RootDirectory: string read FRootDirectory write SetRootDirectory;
    property OnLoaderNotFound: TFileObjectCreatorObj read FOnLoaderNotFound write FOnLoaderNotFound;
    {
      Поиск загруженного объекта, если объект не загружен, то вернет nil
    }
    function FindObject(const FileName: string): IFileObject;
    procedure SetObject(const FileName: string; AObject: IFileObject);
    function GetObject(const FileName: string; Func: TFileObjectCreator; UserValue: Pointer; Options: TStreamOptions = []): IFileObject; overload;
    function GetObject(const FileName: string; Func: TFileObjectCreatorObj = nil; Options: TStreamOptions = []): IFileObject; overload;
    function GetObjectAsync(const FileName: string; Func: TFileObjectCreator; UserValue: Pointer; Options: TStreamOptions = []): IFutureWithSoftCancel; overload;
    function GetObjectAsync(const FileName: string; Func: TFileObjectCreatorObj = nil; Options: TStreamOptions = []): IFutureWithSoftCancel; overload;
    function GetFileStream(const FileName: string; Options: TStreamOptions): TStream; overload;
    {
      Совершенно временное решение, жуткий костыль, так в реальности работать не может
    }
    function GetRealFileName(const FileName: string): string;
    procedure LoadAddOn(const AddOn: string);
    procedure ConnectAddOnAs(const AddOn: IAddOn; Directory: string); overload;
    procedure ConnectAddOnAs(const AddOn, Directory: string); overload;
    {
    ConnectDirectoryAs имеет следующие особенности:
     -если OldName имеется в загруженных аддонах, то данные из этой папки аддонов так же будут доступны при обращении в Directory
     -изначально файл ищется в реальной папке доступной по пути, даже если он пересекается с Directory, потом ищется в
        аддонах по пути и только потом, если нигде не был найден ищется через перенаправление
     -так же добавление папки таким образом сбросит кеш для файлов которые попадают в Directory(пока не реализовано)
     -в случае пересечения папок перенаправления поиск файлов проводится в порядке добавления перенаправлений
    }
    function ConnectDirectoryAs(const OldName, Directory: string): Integer;
    procedure DisconnectDirectory(Directory: string); overload;
    procedure DisconnectAll;
    {
      функция так же создает кеши под все найденные файлы, но пустые, без загрузки самих данных
    }
    function GetFilesInDir(Directory, FileMask: string; List: TStrings; FileType: TFileTypes = ftFiles): Integer;
    {
      TODO: Необходимо исправить сохранение один раз для файлов загруженных через виртуальный каталог
             сейчас оно происходит дважды, т.к. на файл ссылаются в двух местах
    }
    procedure ResetCache(const Directory: string = ''; CacheOperation: TCacheOperation = coDropChanges);
    {
      Root - папка в которой ищутся файлы для сохранения
      Filter - список папок и/или файлов для сохранения, если название заканчивается на слэш, то считается папкой
                путь считается от Root
      Пустые папки не сохраняются
    }
    procedure SaveAsDir(Directory: string; Root: string = ''; Filter: TStrings = nil; OnlyChanged: TChangeStamp = csIgnoreChanges; ResetChanged: Boolean = True);
    procedure SaveAsFile(const FileName: string; Root: string = ''; Filter: TStrings = nil; OnlyChanged: TChangeStamp = csIgnoreChanges; ResetChanged: Boolean = True);
    procedure SaveAsStream(Stream: TStream; Root: string = ''; Filter: TStrings = nil; OnlyChanged: TChangeStamp = csIgnoreChanges; ResetChanged: Boolean = True);
    procedure AddResourceLoader(Func: TFileObjectCreator; FileExt: string; UserValue: Pointer = nil; StreamOptions: TStreamOptions = []); overload;
    procedure AddResourceLoader(Func: TFileObjectCreatorObj; FileExt: string; StreamOptions: TStreamOptions = []); overload;
    property CurrentStamp: TChangeStamp read FCurrentStamp;
    function IsChangedObject(const FO: IFileObject): Boolean; overload; inline;
    function IsChangedObject(const FO: IFileObject; Current: TChangeStamp): Boolean; overload; inline;
    function NextStamp: TChangeStamp;
  end;

  TLowerCaseStringList = class (TStringList)
  protected
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
  end;

var FM: TFileManager;

implementation

{ TFileManager }

function TFileManager.IsChangedObject(const FO: IFileObject): Boolean;
begin
  Result:= FO.GetChangeStamp <> 0;
end;

function TFileManager.IsChangedObject(const FO: IFileObject;
  Current: TChangeStamp): Boolean;
begin
  Result:= FO.GetChangeStamp >= Current;
end;

function TFileManager.AddCacheNote(const RealFile, RelativePath: string; AddOn: PAddonFile; IsVirtual: Boolean): TCacheData;
var IsFind: Boolean;
    str: string;
  procedure InsertInstance(const FileName: string; Cache: TCacheData);
  var Dir: TDirectoryCache;
  begin
    Dir:= FCache.GetDirectory(ExtractFilePath(FileName));
    if Dir = nil then begin
      DebugBreak;
      raise Exception.Create('TFileManager.AddCacheNote: Не найден католог с файлом ' + FileName);
    end;
    Cache.AddDirectory(Dir);
  end;
begin
  IsFind:= False;
  if IsVirtual then begin //если это виртуальный файл(относительная ссылка), то сначала получим его реальную запись
    str:= ExtractRelativePath(FRootDirectory, RealFile);
    IsFind:= FCache.TryFileCache(str, Result);
  end;
  if not IsFind then begin //если новый файл
    Result:= TCacheData.Create(Self);
    if AddOn <> nil then begin
      Result.AddOn:= AddOn.AddOn;
      Result.RealPath:= AddOn.FileName;
    end else
      Result.RealPath:= RealFile;
  end;
  FCache.AddCache(RelativePath, Result);
  //if IsVirtual or not IsFind then //вставим ссылку на текущую запись, если это виртуальный файл
    InsertInstance(RelativePath, Result);
  if IsVirtual and not IsFind then begin //вставим запись о реальном файле, если ее нет
    FCache.AddCache(str, Result);
    InsertInstance(str, Result);
  end;
  //LogAll;
end;

procedure TFileManager.AddResourceLoader(Func: TFileObjectCreatorObj;
  FileExt: string; StreamOptions: TStreamOptions);
var ml: TMaskLoader;
    i: Integer;
begin
  FileExt:= AnsiLowerCase(FileExt);
  if FileExt[1] <> '.' then
    FileExt:= '.' + FileExt;
  for i := 0 to FResourceLoaders.Count - 1 do
    if FResourceLoaders[i].FileExt = FileExt then
      raise Exception.Create('Загрузчик для расширения ''' + FileExt + ''' уже задан');
  ml.LoaderFunction:= Func;
  ml.FileExt:= FileExt;
  ml.StreamOptions:= StreamOptions;
  FResourceLoaders.Add(ml);
end;

procedure TFileManager.AddResourceLoader(Func: TFileObjectCreator;
  FileExt: string; UserValue: Pointer; StreamOptions: TStreamOptions);
var tmp: TFileObjectCreatorObj;
begin
  TMethod(tmp).Code:= Pointer(@Func);
  TMethod(tmp).Data:= UserValue;
  AddResourceLoader(tmp, FileExt, StreamOptions);
end;

procedure TFileManager.ConnectAddOnAs(const AddOn, Directory: string);
var lAddOn: TAddOn;
begin
  lAddOn:= TAddOn.Create;
  ConnectAddOnAs(lAddOn, Directory);
end;

procedure TFileManager.ConnectAddOnAs(const AddOn: IAddOn; Directory: string);
var la: TLoadedAddOn;
begin
  la.AddOn:= AddOn;
  la.Root:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(Directory)));
  FLoadedAddOns.Add(la);
end;

function TFileManager.ConnectDirectoryAs(const OldName, Directory: string): Integer;
var fc: TFolderConnect;
begin
  fc.Root:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(Directory)));
  fc.Folder:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(OldName)));
  Result:= FFolderConnects.Add(fc);
end;

constructor TFileManager.Create(const ARootDirectory: string);
begin
  FRootDirectory:= IncludeTrailingPathDelimiter(AnsiLowerCase(ARootDirectory));
  FResourceLoaders:= TList<TMaskLoader>.Create;
  FLoadedAddOns:= TList<TLoadedAddOn>.Create;
  FFolderConnects:= TList<TFolderConnect>.Create;
  FCache:= TDirectoryCache.Create('');
  FCurrentStamp:= 1;
end;

destructor TFileManager.Destroy;
begin
  ResetCache;
  FCache.Free;
  FResourceLoaders.Free;
  FLoadedAddOns.Free;
  FFolderConnects.Free;
  inherited;
end;

procedure TFileManager.DisconnectAll;
begin
  while FFolderConnects.Count > 0 do
    DisconnectDirectory(FFolderConnects.Count - 1);
end;

procedure TFileManager.DisconnectDirectory(Index: Integer);
begin
  ResetCache(ExtractRelativePath(FRootDirectory, FFolderConnects[Index].Root));
  FFolderConnects.Delete(Index);
end;

procedure TFileManager.DisconnectDirectory(Directory: string);
var i: Integer;
begin
  Directory:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, CollapsePath(AnsiLowerCase(Directory))));
  for i:= FFolderConnects.Count - 1 downto 0 do
    if FFolderConnects[i].Root = Directory then
      DisconnectDirectory(i);
end;

function TFileManager.DoGetObject(const FileName: string;
  Func: TFileObjectCreatorObj; Options: TStreamOptions;
  TrySync: Boolean): IFutureWithSoftCancel;
var Cache: TCacheData;
    RelativePath: string;
    Stream: TStream;
begin
  RelativePath:= CollapsePath(AnsiLowerCase(ReplaceStr(FileName, '/', '\')));
  if not IsRelativePath(RelativePath) then
    RelativePath:= ExtractRelativePath(FRootDirectory, RelativePath);
  if not Assigned(Func) then begin
    Func:= GetLoaderByName(RelativePath, Options);
    if not Assigned(Func) then
      raise Exception.Create('TFileManager.GetObject - не найден загрузчик ресурсов');
  end;
  Cache:= TryCacheNote(RelativePath, False);
  if Cache = nil then
    raise Exception.Create('Файл не найден:' + FileName);
  Cache.Options:= Options;
  if TrySync then
    Result:= Cache.TryLoadSync(Func)
  else
    Result:= Cache.LoadAsync(Func);
end;

function TFileManager.FindObject(const FileName: string): IFileObject;
var Cache: TCacheData;
    RelativePath: string;
begin
  RelativePath:= CollapsePath(AnsiLowerCase(FileName));
  if not IsRelativePath(RelativePath) then
    RelativePath:= ExtractRelativePath(FRootDirectory, RelativePath);
  Result:= nil;
  if FCache.TryFileCache(RelativePath, Cache) then
    Result:= Cache.Load;
end;

function TFileManager.GetFilesInDir(Directory, FileMask: string;
  List: TStrings; FileType: TFileTypes): Integer;

  procedure AddAndCachingFile(const RealPath, RelativePath: string; const AddOn: PAddonFile; IsVirtual: Boolean; List: TStrings; Level: Integer);
  var cache: TCacheData;
  begin
    if not FCache.TryFileCache(RelativePath, cache) then
      cache:= AddCacheNote(RealPath, RelativePath, AddOn, IsVirtual);
    List.AddObject(RelativePath, TObject(Level));
  end;

var ProcMask: TFileMask;

  function LikeMask(const FileName: string): Boolean;
  begin
    Result:= ProcMask.Compare(FileName);
  end;

  function GetNextFolder(const Root, Full: string): string;
  begin

  end;

var Attr: LongWord;

  procedure GetFiles(const Directory, Relative: string; List: TStrings; IsVirtual: Boolean; BeginLevel: Integer);
  var RealPath, PathPart, tmp, checking: string;
      Search: TSearchRec;
      fc: TFolderConnect;
      i, j, first, ofs: Integer;
      addon: TLoadedAddOn;
      addonList: TLowerCaseStringList;
      af: TAddOnFile;
  begin
    if FindFirst(Directory + FileMask, Attr, Search) = 0 then
      repeat
        Search.Name:= AnsiLowerCase(Search.Name);
        if (Search.Attr and faDirectory <> 0) then begin
          if (Search.Name <> '.') and (Search.Name <> '..') then
            if (FileType = ftAllInFolder) or (FileType = ftFolders) then
              List.AddObject(Relative + Search.Name + PathDelim, TObject(BeginLevel))
            else
              GetFiles(Directory + Search.Name + PathDelim, Relative + Search.Name + PathDelim, List, False, BeginLevel + 1);
        end else if FileType <> ftFolders then
          AddAndCachingFile(Directory + Search.Name, Relative + Search.Name, nil, IsVirtual, List, BeginLevel);
      until FindNext(Search) <> 0;
    for i := FLoadedAddOns.Count - 1 downto 0 do begin
      addon:= FLoadedAddOns[i];
      if Directory.StartsWith(addon.Root) then begin
        addonList:= TLowerCaseStringList.Create;
        try
          addonList.CaseSensitive:= False;
          addonList.Sorted:= True;
          addon.AddOn.GetFileList(addonList);
          PathPart:= Copy(Directory, Length(addon.Root) + Low(string));
          addonList.Find(PathPart, first);
          af.AddOn:= addon.AddOn;
          for j := first to addonList.Count - 1 do
          if addonList[j].StartsWith(PathPart) then begin
            tmp:= Copy(addonList[j], Length(PathPart) + Low(string));
            ofs:= Pos(PathDelim, tmp);
            if ofs = Low(string) - 1 then begin
              if (FileType <> ftFolders) and LikeMask(tmp) then begin
                af.FileName:= addonList[j];
                AddAndCachingFile(Directory + tmp, Relative + tmp, @af, IsVirtual, List, BeginLevel);
              end;
            end else if FileType <> ftFiles then begin
              checking:= Copy(tmp, Low(string), ofs - Low(string));
              if ((FileType = ftAllInFolder) or (FileType = ftFolders)) and LikeMask(checking) then
                List.AddObject(Relative + checking + PathDelim, TObject(BeginLevel))
              else begin //ftFilesInSubfolders
                checking:= Copy(tmp, tmp.LastIndexOf(PathDelim) + 1);
                if LikeMask(checking) then begin
                  af.FileName:= addonList[j];
                  AddAndCachingFile(Directory + tmp, Relative + tmp, @af, IsVirtual, List, BeginLevel + tmp.CountChar(PathDelim));
                end;
              end;
            end;
          end else Break;
        finally
          addonList.Free;
        end;
      end else if addon.Root.StartsWith(Directory) then begin
        case FileType of
          ftAllInFolder: ;
          ftFiles: ;
          ftFolders: begin
              tmp:= '';
              for j := Length(Directory) + Low(string) to High(addon.Root) do
                if addon.Root[j] = PathDelim then begin
                  tmp:= Copy(addon.Root, Length(Directory) + Low(string), j - Length(Directory) - Low(string));
                  Break;
                end;
              if tmp = '' then
                tmp:= Copy(addon.Root, Length(Directory) + Low(string));
              List.AddObject(Relative + tmp + PathDelim, TObject(BeginLevel));
            end;
          ftFilesInSubfolders: begin
            addonList:= TLowerCaseStringList.Create;
            try
              addonList.CaseSensitive:= False;
              addonList.Sorted:= True;
              addon.AddOn.GetFileList(addonList);
              tmp:= Copy(addon.Root, Length(Directory) + Low(string));
              af.AddOn:= addon.AddOn;
              for j := 0 to addonList.Count - 1 do begin
                checking:= Copy(addonList[j], addonList[j].LastIndexOf(PathDelim) + 1);
                if LikeMask(checking) then begin
                  af.FileName:= addonList[j];
                  AddAndCachingFile(Directory + tmp + addonList[j],
                      Relative + tmp + addonList[j], @af, IsVirtual, List,
                      BeginLevel + tmp.CountChar(PathDelim) + addonList[j].CountChar(PathDelim));
                end;
              end;
            finally
              addonList.Free;
            end;
          end;
        end;
      end;
    end;
    for fc in FFolderConnects do
      if Pos(fc.Root, Directory) = Low(string) then begin
        Inc(BeginLevel);
        RealPath:= ExpandFileNameEx(fc.Folder, Copy(Directory, Length(fc.Root) + Low(string)));
        GetFiles(RealPath, Relative, List, True, BeginLevel);
      end else if Pos(Directory, fc.Root) = Low(string) then begin
        if (FileType = ftAllInFolder) or (FileType = ftFolders) then begin
          ofs:= Pos(PathDelim, fc.Root, Length(Directory) + Low(string));
          if ofs = 0 then
            ofs:= Length(fc.Root);
          Dec(ofs, Length(Directory) + Low(string));
          List.AddObject(Copy(fc.Root, Length(Directory) + Low(string), ofs) + PathDelim, TObject(BeginLevel))
        end else begin
          Inc(BeginLevel);
          RealPath:= ExpandFileNameEx(fc.Folder, Copy(Directory, Length(fc.Root) + Low(string)));
          GetFiles(RealPath, Relative, List, True, BeginLevel);
        end;
      end;
  end;

var Relative: string;
    Files: TStringList;
begin
  Directory:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(Directory)));
  Relative:= ExtractRelativePath(FRootDirectory, Directory);
  ProcMask:= AnsiLowerCase(FileMask);
  Files:= TStringList.Create;
  try
    Files.Sorted:= True;
    Files.Duplicates:= dupIgnore;
    case FileType of
      ftAllInFolder, ftFolders, ftFilesInSubfolders: Attr:= faDirectory;
    else
      Attr:= 0;
    end;
    GetFiles(Directory, Relative, Files, False, 1);
    List.AddStrings(Files);
    Result:= Files.Count;
  finally
    Files.Free;
  end;
end;

function TFileManager.GetFileStream(Cache: TCacheData; Options: TStreamOptions): TStream;
var s: THandleStream;
  TempPath, TempFile: string;
  BufSize: Cardinal;
begin
  if Cache.AddOn = nil then
    Result:= TFileStream.Create(Cache.RealPath, fmOpenRead or fmShareDenyWrite)
  else begin
    Result:= Cache.AddOn.GetFile(Cache.RealPath, Options);
    //автоматическая обёртка в отдельный файл
    if (soNeedHandle in Options) and not (Result is THandleStream) then begin
      BufSize := GetTempPath(0, nil);
      SetLength(TempPath, BufSize);
      SetLength(TempPath, GetTempPath(BufSize, PChar(TempPath)));
      SetLength(TempFile, MAX_PATH);
      GetTempFileName(PChar(TempPath), AddOnSign, 0, PChar(TempFile));
      s:= TFileStream.Create(CreateFile(PChar(TempFile), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0));
      s.CopyFrom(Result, Result.Size);
      s.Position:= 0;
      Result:= s;
    end;
  end;
end;

function TFileManager.GetFileStream(const FileName: string; Options: TStreamOptions): TStream;
var Cache: TCacheData;
    RelativePath: string;
begin
  RelativePath:= CollapsePath(AnsiLowerCase(FileName));
  if not IsRelativePath(RelativePath) then
    RelativePath:= ExtractRelativePath(FRootDirectory, RelativePath);
  Cache:= TryCacheNote(RelativePath, False);
  if Cache = nil then
    Exit(nil);
  Result:= GetFileStream(Cache, Options);
end;

function TFileManager.GetLoaderByName(const FileName: string;
  var Options: TStreamOptions): TFileObjectCreatorObj;
var Ext: string;
    Loader: TMaskLoader;
begin
  Ext:= AnsiLowerCase(ExtractFileExt(FileName));
  for Loader in FResourceLoaders do
    if Loader.FileExt = Ext then begin
      Options:= Loader.StreamOptions;
      Result:= Loader.LoaderFunction;
      Exit;
    end;
  Result:= nil;
end;

function TFileManager.GetObject(const FileName: string;
  Func: TFileObjectCreator; UserValue: Pointer; Options: TStreamOptions): IFileObject;
var Obj: TFileObjectCreatorObj;
begin
  if Assigned(Func) then begin
    TMethod(Obj).Code:= Pointer(@Func);
    TMethod(Obj).Data:= UserValue;
    Result:= GetObject(FileName, Obj, Options);
  end else begin
    Obj:= nil;
    Result:= GetObject(FileName, Obj, Options);
  end;
end;

function TFileManager.GetObject(const FileName: string;
  Func: TFileObjectCreatorObj; Options: TStreamOptions): IFileObject;
begin
  Result:= DoGetObject(FileName, Func, Options, True).Value;
end;

function TFileManager.GetObjectAsync(const FileName: string;
  Func: TFileObjectCreator; UserValue: Pointer;  Options: TStreamOptions): IFutureWithSoftCancel;
var Obj: TFileObjectCreatorObj;
begin
  if Assigned(Func) then begin
    TMethod(Obj).Code:= Pointer(@Func);
    TMethod(Obj).Data:= UserValue;
    Result:= GetObjectAsync(FileName, Obj, Options);
  end else begin
    Obj:= nil;
    Result:= GetObjectAsync(FileName, Obj, Options);
  end;
end;

function TFileManager.GetObjectAsync(const FileName: string;
  Func: TFileObjectCreatorObj; Options: TStreamOptions): IFutureWithSoftCancel;
begin
  Result:= DoGetObject(FileName, Func, Options, False);
end;

function TFileManager.GetRealFileName(const FileName: string): string;
var Cache: TCacheData;
    RelativePath: string;
begin
  RelativePath:= CollapsePath(AnsiLowerCase(FileName));
  if not IsRelativePath(RelativePath) then
    RelativePath:= ExtractRelativePath(FRootDirectory, RelativePath);
  Cache:= TryCacheNote(RelativePath, False);
  if Cache = nil then
    raise Exception.Create('Файл не найден:' + FileName);
  Result:= Cache.RealPath;
end;

procedure TFileManager.LoadAddOn(const AddOn: string);
var loaded: TLoadedAddOn;
begin
  loaded.AddOn:= TAddOn.Create;
  //loaded.AddOn.OpenFromFile(AddOn);
  FLoadedAddOns.Add(loaded);
end;

procedure TFileManager.LogAll;
  procedure LogFolder(Dir: TDirectoryCache);
  var i: Integer;
  begin
    for i := 0 to Dir.Count - 1 do
      if Dir.Objects[i].ClassType = TDirectoryCache then begin
        LogFolder(TDirectoryCache(Dir.Objects[i]));
      end else begin
        //Log.Add(Dir.Path + Dir[i], TCacheData(Dir.Objects[i]).RealPath);
      end;
  end;
begin
  LogFolder(FCache);
  //Log.Add('');
end;

function TFileManager.NextStamp: TChangeStamp;
begin
  Inc(FCurrentStamp);
  Result:= FCurrentStamp;
end;

procedure TFileManager.OnCacheNotify(Sender: TObject; const Item: TCacheData;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

procedure TFileManager.ResetCache(const Directory: string; CacheOperation: TCacheOperation);
var Dir: TDirectoryCache;
begin
  if Directory <> '' then begin
    Dir := FCache.Directory[Directory];
    if Dir = nil then
      Exit;//Вроде не нужно тут ошибки
      //raise Exception.Create('Не найден путь: ' + Directory);
  end else
    Dir := FCache;
  case CacheOperation of
    coDropNotChanged: Dir.RecursiveDropNotChanged;
    coDropChanges: Dir.Clear(False);
    coSaveChanges: Dir.Clear(True);
  end;
end;

procedure TFileManager.SaveAsDir(Directory, Root: string; Filter: TStrings;
  OnlyChanged: TChangeStamp; ResetChanged: Boolean);
var i: Integer;
    fs: TFileStream;
    FileName, FullRoot, Tmp: string;
    FileList: TStrings;
    Data: TCacheData;
    Dir: TDirectoryCache;
  procedure RecursiveSaveChanged(Dir: TDirectoryCache; OnlyChanged: TChangeStamp);
  var i: Integer;
      c: TCacheData;
      FileName: string;
      fo: IFileObject;
  begin
    for i := 0 to Dir.Count - 1 do begin
      c:= TCacheData(Dir.Objects[i]);
      if c.ClassType = TCacheData then begin
        fo:= c.Load;
        if(fo <> nil) and IsChangedObject(fo, OnlyChanged) then begin
          FileName:= ExpandFileNameEx(FRootDirectory, ExpandFileNameEx(Dir.Path, Dir[i]));
          if Pos(FullRoot, FileName) = Low(string) then begin
            FileName:= ExpandFileNameEx(Directory, ExtractRelativePath(FullRoot, FileName));
            ForceDirectories(ExtractFilePath(FileName));
            fs:= TFileStream.Create(FileName, fmCreate);
            fo.SaveToStream(fs);
            FreeAndNil(fs);
            {$MESSAGE WARN 'Необходима проверка и сброс файлов в которые сохраняем'}
            {$MESSAGE WARN 'Отсутствует фильтрация'}
          end;
        end;
      end else if c.ClassType = TDirectoryCache then
        RecursiveSaveChanged(TDirectoryCache(c), OnlyChanged);
    end;
  end;
begin
  //сохранять всю папку с игрой можно только для измененных файлов
  if (OnlyChanged = csIgnoreChanges) and (Root = '') then
    Exit;

  Root:= AnsiLowerCase(Root);
  FullRoot:= ExpandFileNameEx(FRootDirectory, Root);
  Directory:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(Directory)));
  if Filter <> nil then
    for i := 0 to Filter.Count - 1 do
      Filter[i]:= ExpandFileNameEx(FullRoot, AnsiLowerCase(Filter[i]));
  fs:= nil;
  try
    if OnlyChanged <> csIgnoreChanges then begin
      if OnlyChanged = csAllChanges then
        OnlyChanged:= 1; //если все изменения, то 1 в самый раз, ведь самое раннее изменение = 1
      Dir:= FCache.Directory[Root];
      if Dir <> nil then
        RecursiveSaveChanged(Dir, OnlyChanged);
    end else begin
      FileList:= TStringList.Create;
      if GetFilesInDir(FullRoot, '*', FileList, ftFilesInSubfolders) > 0 then begin
        for FileName in FileList do begin
          if not FCache.TryFileCache(FileName, Data) then
            raise Exception.Create('Файл не найден: ' + FileName);
          if (Data.Load <> nil) and IsChangedObject(Data.Load) then begin
            Tmp:= ExpandFileNameEx(Directory, ExtractRelativePath(FullRoot, ExpandFileNameEx(FRootDirectory, FileName)));
            ForceDirectories(ExtractFilePath(Tmp));
            fs:= TFileStream.Create(Tmp, fmCreate);
            Data.Load.SaveToStream(fs);
            FreeAndNil(fs);
            {$MESSAGE WARN 'Необходима проверка и сброс файлов в которые сохраняем'}
            {$MESSAGE WARN 'Отсутствует фильтрация'}
          end else if Data.AddOn = nil then begin   
            Tmp:= ExpandFileNameEx(Directory, ExtractRelativePath(FullRoot, ExpandFileNameEx(FRootDirectory, FileName)));
            if Tmp <> Data.RealPath then begin
              ForceDirectories(ExtractFilePath(Tmp));
              CopyFile(PChar(Data.RealPath), PChar(Tmp), False);
              SetFileAttributes(PChar(Tmp), GetFileAttributes(PChar(Tmp)) and not FILE_ATTRIBUTE_READONLY);
            end;
          end else begin
            {$MESSAGE WARN 'Загрузка файлов из аддонов пока не поддерживается'}
          end;
        end;
      end;
      FileList.Free;
    end;
  finally
    fs.Free;
  end;
end;

procedure TFileManager.SaveAsFile(const FileName: string; Root: string;
  Filter: TStrings; OnlyChanged: TChangeStamp; ResetChanged: Boolean);
begin

end;

procedure TFileManager.SaveAsStream(Stream: TStream; Root: string;
  Filter: TStrings; OnlyChanged: TChangeStamp; ResetChanged: Boolean);
begin

end;

procedure TFileManager.SetObject(const FileName: string; AObject: IFileObject);
var Cache: TCacheData;
    RelativePath: string;
begin
  RelativePath:= CollapsePath(AnsiLowerCase(ReplaceStr(FileName, '/', '\')));;
  if not IsRelativePath(RelativePath) then
    RelativePath:= ExtractRelativePath(FRootDirectory, RelativePath);
{$MESSAGE WARN 'Ну и фигня, нужно поправить, а то будет бардак с несколькими версиями файлов'}
  raise Exception.Create('Error Message');
  {if FCache.TryFileCache(RelativePath, Cache) then begin
    Cache.FileObject:= AObject;
  end else begin
    Cache:= TryCacheNote(RelativePath, True);
    Cache.FileObject:= AObject;
  end;}
end;

procedure TFileManager.SetRootDirectory(const ARootDirectory: string);
begin
  FRootDirectory:= IncludeTrailingPathDelimiter(AnsiLowerCase(ARootDirectory));
end;

function TFileManager.TryCacheNote(const RelativePath: string; CreateNew: Boolean): TCacheData;
  function GetAddOnCache(const RelativePath, RealPath: string; IsVirtual: Boolean): TCacheData;
  var i: Integer;
      lao: TLoadedAddOn;
      addOnFile: string;
      af: TAddOnFile;
  begin
    for i := FLoadedAddOns.Count - 1 downto 0 do begin
      lao:= FLoadedAddOns[i];
      af.AddOn:= lao.AddOn;
      if RealPath.StartsWith(lao.Root) then begin
        af.FileName:= Copy(RealPath, Length(lao.Root) + 1);
        if lao.AddOn.FileExist(af.FileName) then begin
          Result:= AddCacheNote(RealPath, RelativePath, @af, IsVirtual);
          Exit;
        end;
      end;
    end;
    Result:= nil;
  end;
var FullDir, RealPath: string;
    fc: TFolderConnect;
    i: Integer;
begin
  if not FCache.TryFileCache(RelativePath, Result) then begin
    FullDir:= ExpandFileNameEx(FRootDirectory, RelativePath);
    {$MESSAGE WARN 'Создание файлов в каталоге аддона создаёт реальный файл'}
    if FileExists(FullDir) then
      Result:= AddCacheNote(FullDir, RelativePath, nil, False)
    else begin
      Result:= GetAddOnCache(RelativePath, FullDir, False);
      if Result = nil then begin
        for i := 0 to FFolderConnects.Count - 1 do begin
          fc:= FFolderConnects[i];
          if FullDir.StartsWith(fc.Root) then begin //это виртуальный путь
            RealPath:= ExpandFileNameEx(fc.Folder, Copy(FullDir, Length(fc.Root) + 1));
            if FileExists(RealPath) then
              Exit(AddCacheNote(RealPath, RelativePath, nil, True));
            Result:= GetAddOnCache(RelativePath, RealPath, True);
            if Result <> nil then
              Exit
            else if CreateNew then
              Exit(AddCacheNote(RealPath, RelativePath, nil, True));
          end;
        end;
        if CreateNew then
          Exit(AddCacheNote(FullDir, RelativePath, nil, False));
      end;
    end;
  end;
end;

{ TFileObjectDelegate }

constructor TFileObjectDelegate.Create(AOwner: TObject);
begin
  FOwner:= AOwner;
end;

destructor TFileObjectDelegate.Destroy;
begin
  if (FOwner <> nil) and (FOwner <> Self) then
    FOwner.Destroy;
  inherited;
end;

function TFileObjectDelegate.GetChangeStamp: TChangeStamp;
begin
  Result:= FChanged;
end;

function TFileObjectDelegate.GetObject: TObject;
begin
  Result:= FOwner;
end;

procedure TFileObjectDelegate.ResetChangeStamp;
begin
  FChanged:= 0;
end;

procedure TFileObjectDelegate.SetChangeStamp;
begin
  FChanged:= FM.CurrentStamp;
end;

{ TFileObjectAggregated }

constructor TFileObjectAggregated.Create(AOwner: TObject;
  const Controller: IInterface);
begin
  FOwner:= AOwner;
  inherited Create(Controller);
end;

function TFileObjectAggregated.GetChangeStamp: TChangeStamp;
begin
  Result:= FChanged;
end;

function TFileObjectAggregated.GetObject: TObject;
begin
  Result:= FOwner;
end;

procedure TFileObjectAggregated.ResetChangeStamp;
begin
  FChanged:= 0;
end;

procedure TFileObjectAggregated.SetChangeStamp;
begin
  FChanged:= FM.CurrentStamp;
end;

{ TDirectoryCache }

procedure TDirectoryCache.AddCache(const Name: string; Cache: TCacheData);
var Dir, b: TDirectoryCache;
    Dirs: TStringDynArray;
    FullName: string;
    i, j: Integer;
begin
  Cache._AddRef;
  try
    FullName:= AnsiLowerCase(Name);

    Dirs:= SplitString(FullName, PathDelim);

    Dir:= Self;


    for i := 0 to High(Dirs) - 1 do begin
      b:= Dir;
      MonitorEnter(b);
      try
        if Dir.Find(Dirs[i], j) then begin
          if Dir.Objects[j].ClassType <> TDirectoryCache then begin
            DebugBreak;
            raise Exception.Create('Есть файл с названием: ' + Dir.Path + Dirs[i] + '. Нельзя создать папку с таким же названием.');
          end;
          Dir:= TDirectoryCache(Dir.Objects[j])
        end else begin
          Dir:= TDirectoryCache(Dir.Objects[
              Dir.AddObject(Dirs[i], TDirectoryCache.Create(Dir.Path + Dirs[i] + PathDelim))]);
        end;
      finally
        MonitorExit(b);
      end;
    end;

    MonitorEnter(Dir);
    try
      {if Dir.Find(Dirs[High(Dirs)], i) then begin
        DebugBreak;
        raise Exception.Create('Уже есть запись с таким названием');
        //TCacheData(Dir.Objects[i])._Release;
        //Dir.Objects[i]:= Cache;
      end else}
      Dir.AddObject(Dirs[High(Dirs)], Cache);
    finally
      MonitorExit(Dir);
    end;
  except
    Cache._Release;
    raise;
  end;
end;

function TDirectoryCache.AddObject(const S: string; Value: TObject): Integer;
var item: TPair<string, TObject>;
begin
  item.Key:= S;
  item.Value:= Value;
  if TArray.BinarySearch<TPair<string, TObject>>(FData.List, item, Result, FData.Comparer, 0, FData.Count) then
    raise Exception.Create('Уже есть запись с таким названием');
  FData.Insert(Result, item);
end;

procedure TDirectoryCache.Clear(DoSave: Boolean);
var i, Index: Integer;
    Cache: TCacheData;
    fo: IFileObject;
    temp: TListRecord<TPair<string, TCacheData>>;
    dirs: TListRecord<TDirectoryCache>;
    Stream: TStream;
  Instance: TCacheInstance;
  tmp: TCacheInstance;
  good: Boolean;
begin
  MonitorEnter(Self);
  try
    temp.Create(Count);
    dirs.Create(Count);
    for i := Count - 1 downto 0 do begin
      if Objects[i].ClassType = TCacheData then begin
        Cache:= TCacheData(Objects[i]);
        temp.Add(TPair<string, TCacheData>.Create(Items[i], Cache));
        Cache._AddRef;
      end else
        dirs.Add(TDirectoryCache(Objects[i]));
      FData.Delete(i);
    end;
  finally
    MonitorExit(Self);
  end;

  for i := 0 to dirs.Count - 1 do
    dirs[i].Clear(DoSave);

  for i := 0 to temp.Count - 1 do begin
    Cache:= temp[i].Value;
    if DoSave then begin
      MonitorEnter(Cache);
      try
        fo:= Cache.Load;
        if (fo <> nil) and (FO.GetChangeStamp <> 0) and not Cache.IsSaved then begin
          if Cache.AddOn = nil then begin
            Stream:= TFileStream.Create(Cache.RealPath, fmCreate);
          end else begin
            {$MESSAGE WARN 'Сохранение файлов из аддонов пока не поддерживается'}
          end;
          fo.SaveToStream(Stream);
          Stream.Free;
          Cache.IsSaved:= True;
        end;
      finally
        MonitorExit(Cache);
      end;
    end;
    Cache.ClearCache(Self, temp[i].Key);
    Cache._Release;
  end;
end;

class constructor TDirectoryCache.Create;
begin
  FDirectoryComparer:= TDirectoryComparer.Create;
end;

constructor TDirectoryCache.Create(const APath: string);
begin
  inherited Create;
  FData.Create(DirectoryComparer, 4);
  if APath <> '' then
    FPath:= IncludeTrailingPathDelimiter(AnsiLowerCase(APath));
end;

class destructor TDirectoryCache.Destroy;
begin
  FDirectoryComparer:= nil;
end;

destructor TDirectoryCache.Destroy;
begin
  Clear(False);
  inherited;
end;

function TDirectoryCache.Find(const S: string; var Index: Integer): Boolean;
var item: TPair<string, TObject>;
begin
  item.Key:= s;
  Result:= TArray.BinarySearch<TPair<string, TObject>>(FData.List, item, Index, FData.Comparer, 0, FData.Count);
end;

function TDirectoryCache.GetCount: Integer;
begin
  Result:= FData.Count;
end;

function TDirectoryCache.GetDirectory(
  const List: array of string): TDirectoryCache;
var i, j: Integer;
    b: TDirectoryCache;
begin
  Result:= Self;

  for i := 0 to High(List) do begin
    b:= Result;
    try
      MonitorEnter(b);
      if Result.Find(List[i], j) and (Result.FData[j].Value.ClassType = TDirectoryCache) then
        Result:= TDirectoryCache(Result.FData[j].Value)
      else
        Exit(nil);
    finally
      MonitorExit(b);
    end;
  end;
end;

function TDirectoryCache.GetDirectory(const Name: string): TDirectoryCache;
var i, j: Integer;
    FullName: string;
    Dirs: TStringDynArray;
begin
  if Name = '' then
    Exit(Self);

  FullName:= AnsiLowerCase(Name);

  Dirs:= SplitString(FullName, PathDelim);

  if FullName[High(FullName)] = PathDelim then
    SetLength(Dirs, Length(Dirs) - 1);

  Result:= GetDirectory(Dirs);
end;

function TDirectoryCache.GetFileCache(const Name: string): TCacheData;
var Dir: TDirectoryCache;
    p: Integer;
    FileName: string;
begin
  p:= LastDelimiter(PathDelim, Name);
  if p > Low(string) then begin
    Dir:= GetDirectory(Copy(Name, 1, p - Low(string)));
    if Dir = nil then
      Exit(nil);
  end else
    Dir:= Self;
  FileName:= AnsiLowerCase(Copy(Name, p + Low(string), Length(Name)));
  Result:= nil;
  MonitorEnter(Dir);
  try
    if Dir.Find(FileName, p) and (Dir.Objects[p].ClassType = TCacheData) then
      Result:= TCacheData(Dir.Objects[p]);
  finally
    MonitorExit(Dir);
  end;
end;

function TDirectoryCache.GetItem(Index: Integer): string;
begin
  Result:= FData[Index].Key;
end;

function TDirectoryCache.GetObject(Index: Integer): TObject;
begin
  Result:= FData[Index].Value;
end;

procedure TDirectoryCache.RecursiveDropNotChanged;
var i, Index: Integer;
    Cache: TCacheData;
    fo: IFileObject;
    temp: TListRecord<TPair<string, TCacheData>>;
    dirs: TListRecord<TDirectoryCache>;
  Instance: TCacheInstance;
  tmp: TCacheInstance;
  good: Boolean;
begin
  MonitorEnter(Self);
  try
    temp.Create(Count);
    dirs.Create(Count);
    for i := Count - 1 downto 0 do
      if Objects[i].ClassType = TCacheData then begin
        Cache:= TCacheData(Objects[i]);
        fo:= Cache.Load;
        if (fo = nil) or (FO.GetChangeStamp = 0) then begin
          temp.Add(TPair<string, TCacheData>.Create(Items[i], Cache));
          Cache._AddRef;
          FData.Delete(i);
        end;
      end else
        dirs.Add(TDirectoryCache(Objects[i]));
  finally
    MonitorExit(Self);
  end;

  for i := 0 to dirs.Count - 1 do
    dirs[i].RecursiveDropNotChanged;

  for i := 0 to temp.Count - 1 do begin
    Cache:= temp[i].Value;
    Cache.ClearCache(Self, temp[i].Key);
    Cache._Release;
  end;
end;

procedure TDirectoryCache.SetDirectory(const Name: string;
  const Value: TDirectoryCache);
begin

end;

procedure TDirectoryCache.SetFileCache(const Name: string;
  const Value: TCacheData);
begin

end;

function TDirectoryCache.TryFileCache(const Name: string;
  var Cache: TCacheData): Boolean;
begin
  Cache:= FileCache[Name];
  Result:= Cache <> nil;
end;

{ TCacheInstance }

constructor TCacheInstance.Create(ANext: TCacheInstance;
  ADirectoryCache: TDirectoryCache);
begin
  Next:= ANext;
  DirectoryCache:= ADirectoryCache;
end;

{ TAddOn }

procedure TAddOn.AddNewFile(const Name: string; Obj: IFileObject);
begin

end;

constructor TAddOn.Create;
begin
  FFileNames:= TStringList.Create;
  FFileNames.CaseSensitive:= False;
  FFileNames.Sorted:= True;
end;

function TAddOn.CreateNewFile(const FileName: string;
  FileSize: Integer): TStream;
begin

end;

destructor TAddOn.Destroy;
begin
  FFileNames.Free;
  {for i:= 0 to High(FFileTables) do
    FreeMem(FFileTables[i]);}
  FreeMem(FHeader);
  inherited;
end;

function TAddOn.FileExist(const FileName: string): Boolean;
begin

end;

function TAddOn.GetFile(const FileName: string; Options: TStreamOptions): TStream;
begin

end;

procedure TAddOn.GetFileList(List: TStrings);
begin

end;

procedure TAddOn.LoadFromStream(Stream: TStream);
begin

end;

function TAddOn.OpenFile(const FileName: string): TStream;
begin

end;

procedure TAddOn.OpenFromFile(const FileName: string; Mode: Word);
var Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, Mode);
  OpenFromOwnedStream(Stream);
end;

procedure TAddOn.OpenFromOwnedStream(Stream: TStream);
{var tmp: TAddOnHeader;
    BlockPos: LongWord;
    i: Integer;  }
begin
  {if FStream <> nil then
    raise Exception.Create('AddOn already initialised!');
  FStream:= Stream;
  FStream.ReadBuffer(tmp, SizeOf(tmp));
  if tmp.Sign <> AddOnSign then
    raise Exception.Create('File not supported archive.');
  GetMem(FHeader, tmp.BlockSize * MinBlockSize);
  FStream.Position:= 0;
  FStream.ReadBuffer(FHeader^, tmp.BlockSize * MinBlockSize);
  FFirstFileTable:= Pointer(Integer(FHeader) + SizeOf(TAddOnHeader));
  SetLength(FFileTables, FHeader.TableCount);
  BlockPos:= FFirstFileTable.NextTableBlockNum + 1;
  for i:= 0 to FHeader.TableCount - 1 do begin
    FStream.Position:= BlockPos * tmp.BlockSize;
    GetMem(FFileTables[i], tmp.BlockSize * MinBlockSize);
    FStream.ReadBuffer(FFileTables[i]^, tmp.BlockSize * MinBlockSize);
    BlockPos:= FFileTables[i].NextTableBlockNum + 1;
  end; }
end;

procedure TAddOn.SaveToStream(Stream: TStream);
begin

end;

{ TLowerCaseStringList }

procedure TLowerCaseStringList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  inherited InsertItem(Index, AnsiLowerCase(S), AObject);
end;

{ TSimpleFileObject }

function TSimpleFileObject.GetChangeStamp: TChangeStamp;
begin
  Result:= FChanged;
end;

function TSimpleFileObject.GetObject: TObject;
begin
  Result:= Self;
end;

procedure TSimpleFileObject.ResetChangeStamp;
begin
  FChanged:= 0;
end;

procedure TSimpleFileObject.SaveToStream(Stream: TStream);
begin

end;

{ TCacheData }

procedure TCacheData.AddDirectory(Dir: TDirectoryCache);
begin
  MonitorEnter(Self);
  try
    Instance:= TCacheInstance.Create(Instance, Dir);
  finally
    MonitorExit(Self);
  end;
end;

procedure TCacheData.ClearCache(Initiator: TDirectoryCache; const Name: string);
var local: TCacheInstance;
  tmp: TCacheInstance;
  Index: Integer;
begin
  if MonitorTryEnter(Self) then
  try
    local:= Instance;
    while local <> nil do begin
      if (local.DirectoryCache <> Initiator) then begin
        MonitorEnter(local.DirectoryCache);
        try
          if local.DirectoryCache.Find(Name, Index) then begin
            local.DirectoryCache.FData.Delete(Index);
            _Release;
          end;
        finally
          MonitorExit(local.DirectoryCache);
        end;
      end;
      tmp:= local;
      local:= local.Next;
      tmp.Free;
    end;
    Instance:= nil;
    _Release;
  finally
    MonitorExit(Self);
  end;
end;

constructor TCacheData.Create(AManager: TFileManager);
begin
  FManager:= AManager;
end;

function TCacheData.CreateStream: TStream;
begin
  Result:= FManager.GetFileStream(Self, Options);
end;

function TCacheData.IsCreated: Boolean;
begin
  Result:= FFileObject <> nil;
end;

function TCacheData.Load: IFileObject;
begin
  if (FFileObject = nil) or (FTask = nil) then
    Result:= nil
  else
    Result:= FTask.Value;
end;

function TCacheData.LoadAsync(ACreator: TFileObjectCreatorObj): IFutureWithSoftCancel;
begin
  TMonitor.Enter(Self);
  if FTask = nil then begin
    if FFileObject = nil then
      FFileObject:= ACreator(FManager, RealPath);
    FTask:= TFileObjectLoader.Future(Self);
  end;
  TMonitor.Exit(Self);
  Result:= FTask;
end;

function TCacheData.TryLoadSync(
  ACreator: TFileObjectCreatorObj): IFutureWithSoftCancel;
begin
  TMonitor.Enter(Self);
  if FTask = nil then begin
    if FFileObject = nil then
      FFileObject:= ACreator(FManager, RealPath);
    FTask:= TFileObjectLoader.TrySync(Self);
  end;
  TMonitor.Exit(Self);
  Result:= FTask;
end;

{ TFileObjectLoader }

class constructor TFileObjectLoader.Create;
begin
  TThreadPool.GetObjectCachesPublic.AddObjectCache(TFileObjectLoader);
end;

constructor TFileObjectLoader.Create(Sender: TCacheData; APool: TThreadPool);
begin
  FResult:= Sender.FFileObject;
  inherited Create(Sender, RunEvent, nil, APool, nil, []);
end;

class function TFileObjectLoader.CurrentTask: IFutureWithSoftCancel;
var t: ITask;
begin
  t:= TTask.CurrentTask;
  Result:= t as IFutureWithSoftCancel;
end;

class function TFileObjectLoader.Future(AFileData: TCacheData;
  APool: TThreadPool): IFutureWithSoftCancel;
begin
  Result := IFutureWithSoftCancel(TFileObjectLoader.Create(AFileData, APool).Start);
end;

function TFileObjectLoader.GetValue: IFileObject;
begin
  Wait;
  Result:= FResult;
end;

function TFileObjectLoader.IsSoftCanceled: Boolean;
begin
  Result:= FSoftCanceled;
end;

procedure TFileObjectLoader.RunEvent(Sender: TObject);
var stream: TStream;
begin
  stream:= TCacheData(Sender).CreateStream;
  try
    if IsSoftCanceled then
      Exit;
    FResult.ReloadObject(stream);
  finally
    if not (soNeedDeffered in TCacheData(Sender).Options) then
      stream.Free;
  end;
end;

procedure TFileObjectLoader.SoftCancel;
begin
  FSoftCanceled:= True;
end;

function TFileObjectLoader.Start: IFuture<IFileObject>;
begin
  inherited Start;
  Result := Self as IFutureWithSoftCancel;
end;

class function TFileObjectLoader.TrySync(AFileData: TCacheData;
  APool: TThreadPool): IFutureWithSoftCancel;
var obj: TFileObjectLoader;
begin
  //if CurrentTask <> nil then begin
    obj := TFileObjectLoader.Create(AFileData, APool);
    Result:= obj;
    obj.RunEvent(AFileData);
    obj.Complete(False);
  //end else
  //  Result:= Future(AFileData, APool);
end;

{ TThreadPoolHelper }

class function TThreadPoolHelper.GetObjectCachesPublic: TObjectCaches;
begin
  Result:= Self.ObjectCaches;
end;

{ TDirectoryComparer }

function TDirectoryComparer.Compare(const Left,
  Right: TPair<string, TObject>): Integer;
begin
  Result := CompareStr(Left.Key, Right.Key);
end;

end.
