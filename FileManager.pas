﻿unit FileManager;
{$IFDEF DEBUG}
  {$RANGECHECKS ON}
{$ELSE}
  {$RANGECHECKS OFF}
{$ENDIF}
{
  Сохранение измененных файлов можно сделать только с помощью функции SaveAsDir

TODO:
  Для кеша не подходит использование TDictionary - требуется переделать на другой класс
}

interface

uses Windows, SysUtils, Classes, System.Generics.Collections, System.Generics.Defaults,
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
      soNeedHandle,
      soNeedWrite
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

  IObjectContainer = interface
    procedure FileUpdated;
    procedure FileDeleted;
  end;

  TDelegatedInterface = class (TInterfacedObject)
  private
    FOwner: TObject;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
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

  TFileChanged = procedure (ACache: TCacheData) of object;

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

  TFileInfo = class;

  TNamedObject = class
  private
    FName: string;
  protected
  public
    property Name: string read FName;
  end;

  TNamedObjectRec = record
    ObjectClass: TClass;
    Name: string;
  end;

  TFileLink = class (TNamedObject)
  private
    FFileInfo: TFileInfo;
  protected
  public
    constructor Create(AFileInfo: TFileInfo);
    function GetFileStream(Options: TStreamOptions): TStream;
    function HasSubscribers: Boolean;
    procedure DoUpdateFile;
  end;

  TFileInfo = class
  private
    FAddOn: IAddOn;
    FRealPath: string; //для аддонов содержит название в рамках аддона
  public
    property AddOn: IAddOn read FAddOn;
    property RealPath: string read FRealPath;
    constructor Create(const AAddOn: IAddOn; const ARealPath: string);
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
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

  TNamedObjectComparer = class (TInterfacedObject, IComparer<TNamedObject>)
  public
    function Compare(const Left, Right: TNamedObject): Integer;
  end;

  TDirectoryCache = class (TNamedObject)
  private class var
    FDirectoryComparer: IComparer<TNamedObject>;
  public
    class property DirectoryComparer: IComparer<TNamedObject> read FDirectoryComparer;
    class constructor Create;
  private
    FFolders: TListRecord<TDirectoryCache>;
    FFiles: TListRecord<TFileLink>;
    function GetDirectory(const Name: string): TDirectoryCache; overload;
    function GetDirectory(const List: array of string): TDirectoryCache; overload;
    function GetFileCache(const Name: string): TFileLink;
    function GetDirectoryCache(AIndex: Integer): TDirectoryCache; inline;
    function GetFileLink(AIndex: Integer): TFileLink; inline;
    function GetFilesCount: Integer; inline;
    function GetFoldersCount: Integer; inline;
  strict protected
    function FindFolder(const S: string; var Index: Integer): Boolean;
    function FindFile(const S: string; var Index: Integer): Boolean;
  protected
  public
    procedure RecursiveDropNotChanged;
    constructor Create(const APath: string);
    property Directory[const Name: string]: TDirectoryCache read GetDirectory;
    property FileCache[const Name: string]: TFileLink read GetFileCache;
    property FilesCount: Integer read GetFilesCount;
    property FoldersCount: Integer read GetFoldersCount;
    property Files[Index: Integer]: TFileLink read GetFileLink;
    property Folders[Index: Integer]: TDirectoryCache read GetDirectoryCache;
    function TryFileLink(const Name: string; var Cache: TFileLink): Boolean;
    procedure AddFileLink(const Name: string; Cache: TFileLink);
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
    FFileLinks: TObjectDictionary<TFileInfo, TList<TFileLink>>;
    FFileObjects: TObjectDictionary<TFileInfo, TList<IFileObject>>;
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
    function AddFileLink(const RealFile, RelativePath: string; AddOn: PAddonFile; IsVirtual: Boolean = False): TFileLink;
    function TryCacheNote(const RelativePath: string; CreateNew: Boolean = False): TCacheData;
    function TryFileLink(const RelativePath: string; CreateNew: Boolean = False): TFileLink;
    function GetFileStream(Cache: TFileInfo; Options: TStreamOptions): TStream; overload;
    function DoGetObject(const FileName: string; Func: TFileObjectCreatorObj; Options: TStreamOptions; TrySync: Boolean): IFutureWithSoftCancel;
    procedure DoResetFile(ACurrentCache: TDirectoryCache; ANewFolder: TFolderConnect);

    procedure DropNotChanged(Dir: TDirectoryCache);
    procedure DropAll(Dir: TDirectoryCache);
    procedure DropWithSave(Dir: TDirectoryCache);
  public
    procedure LogAll;
    function TranslateFileName(const FileName: string): string;
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
    function GetFileLink(const FileName: string): TFileLink;
    {
      Совершенно временное решение, жуткий костыль, так в реальности работать не может
    }
    function GetRealFileName(const FileName: string): string;
    procedure LoadAddOn(const AddOn: string);
    {
      Подключение новых папок или аддонов имеет следующие особенности:
       -если OldName (для папок) имеется в загруженных аддонах, то данные из этой папки аддонов так же будут доступны при обращении в Directory
       -изначально файл ищется в новой папке доступной по пути, потом ищется в
          аддонах по пути и только потом, если нигде не был найден ищется в реальном пути и аддонах подключенных к реальному пути
       -так же добавленые файлы таким образом сбросит кеш для файлов которые попадают в Directory(пока не реализовано)
       -в случае пересечения папок перенаправления приоритет имею последние добавленные перенаправления
       -перегрузка папки которой перегружают другую папку не влияет на изначальную перегрузку.
        Пример:
        у нас есть файлы:
          test/main.txt
          test/First/main.txt
          test/Second/main.txt
        перегружаем test через test/First, теперь test/main.txt = test/First/main.txt
        если перегрузить test/First через test/Second, то для test/main.txt ничего не поменяется,
        но test/First/main.txt = test/Second/main.txt
        порядок перегрузки ничего не изменит
       -если подключается папка в которой уже находит аддон, то будет учитываться только содержимое папки
          без аддона который в неё подключен
    }
    procedure ConnectAddOnAs(const AddOn: IAddOn; Directory: string); overload;
    procedure ConnectAddOnAs(const AddOn, Directory: string); overload;
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

const
  WrongPathDelim  = {$IFDEF MSWINDOWS} '/'; {$ELSE} '\'; {$ENDIF}

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

function TFileManager.AddFileLink(const RealFile, RelativePath: string; AddOn: PAddonFile; IsVirtual: Boolean): TFileLink;
var IsFind: Boolean;
    str: string;
    fInfo: TFileInfo;
    l: TList<TFileLink>;
begin
  IsFind:= False;
  if AddOn <> nil then
    fInfo:= TFileInfo.Create(AddOn.AddOn, AddOn.FileName)
  else
    fInfo:= TFileInfo.Create(nil, RealFile);

  if FFileLinks.TryGetValue(fInfo, l) then begin
    fInfo.Destroy; //удаляем новый объект и подбираем старый, дабы не дублировать
    fInfo:= l[0].FFileInfo;
  end else begin
    l:= TList<TFileLink>.Create;
    FFileLinks.Add(fInfo, l);
  end;
  Result:= TFileLink.Create(fInfo);
  l.Add(Result);

  FCache.AddFileLink(RelativePath, Result);
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
  la.Root:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, TranslateFileName(Directory)));
  FLoadedAddOns.Add(la);
end;

function TFileManager.ConnectDirectoryAs(const OldName, Directory: string): Integer;
var fc: TFolderConnect;
    dir: TDirectoryCache;
    path: string;
begin
  fc.Root:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, TranslateFileName(Directory)));
  fc.Folder:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, TranslateFileName(OldName)));
  Result:= FFolderConnects.Add(fc);
  path:= fc.Root;
  if path.StartsWith(FRootDirectory) then
    path:= Copy(path, Length(FRootDirectory) + 1);
  dir:= FCache.Directory[path];
  if dir <> nil then
    DoResetFile(dir, fc);
end;

constructor TFileManager.Create(const ARootDirectory: string);
begin
  FRootDirectory:= IncludeTrailingPathDelimiter(AnsiLowerCase(ReplaceStr(ARootDirectory, WrongPathDelim, PathDelim)));
  FFileLinks:= TObjectDictionary<TFileInfo, TList<TFileLink>>.Create([doOwnsKeys, doOwnsValues]);
  FResourceLoaders:= TList<TMaskLoader>.Create;
  FLoadedAddOns:= TList<TLoadedAddOn>.Create;
  FFolderConnects:= TList<TFolderConnect>.Create;
  FCache:= TDirectoryCache.Create('');
  FCurrentStamp:= 1;
end;

destructor TFileManager.Destroy;
begin
  ResetCache;
  FFileLinks.Free;
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
  Directory:= IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, TranslateFileName(Directory)));
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
  RelativePath:= TranslateFileName(FileName);
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

procedure TFileManager.DoResetFile(ACurrentCache: TDirectoryCache; ANewFolder: TFolderConnect);
  procedure DeepProcess(const Path: string; Dir: TDirectoryCache);
  var i: Integer;
      oldCache: TCacheData;
      str: string;
      fInfo: TFileInfo;
      l: TList<TFileLink>;
      f: TFileLink;
  begin
    for i := Dir.FilesCount - 1 downto 0 do
      if Dir.FFiles[i].HasSubscribers then begin
        str:= ANewFolder.Folder + Path + Dir.FFiles[i].Name;
        if FileExists(str) then begin
          f:= Dir.FFiles[i];
          fInfo:= TFileInfo.Create(nil, str);
          if FFileLinks.TryGetValue(fInfo, l) then begin
            fInfo.Destroy; //удаляем новый объект и подбираем старый, дабы не дублировать
            fInfo:= l[0].FFileInfo;
          end else begin
            l:= TList<TFileLink>.Create;
            FFileLinks.Add(fInfo, l);
          end;
          if f.FFileInfo <> fInfo then begin
            with FFileLinks[f.FFileInfo] do begin
              Remove(f);
              if Count = 0 then
                FFileLinks.Remove(f.FFileInfo);
            end;
            f.FFileInfo:= fInfo;
            l.Add(f);
          end;
          f.DoUpdateFile;
        end;
      end else
        Dir.FFiles.Delete(i);
    for i := 0 to Dir.FoldersCount - 1 do
      DeepProcess(Path + Dir.Folders[i].Name + PathDelim, Dir.Folders[i])
  end;
begin
  DeepProcess('', ACurrentCache);
end;

procedure TFileManager.DropAll(Dir: TDirectoryCache);
begin

end;

procedure TFileManager.DropNotChanged(Dir: TDirectoryCache);
var i, Index: Integer;
    Cache: TCacheData;
    fo: IFileObject;
    temp: TListRecord<TPair<string, TCacheData>>;
    dirs: TListRecord<TDirectoryCache>;
  Instance: TCacheInstance;
  tmp: TCacheInstance;
  good: Boolean;
begin
  {$MESSAGE WARN 'fix it'}
  {MonitorEnter(Dir);
  try
    temp.Create(Dir.FilesCount);
    dirs.Create(Dir.FoldersCount);
    for i := Dir.FilesCount - 1 downto 0 do
        Cache:= TCacheData(Objects[i]);
        fo:= Cache.Load;
        if (fo = nil) or (FO.GetChangeStamp = 0) then begin
          temp.Add(TPair<string, TCacheData>.Create(Items[i], Cache));
          Cache._AddRef;
          FData.Delete(i);
        end;
    for i := Dir.FoldersCount - 1 downto 0 do
      dirs.Add(Dir.Folders[i]);
  finally
    MonitorExit(Dir);
  end;

  for i := 0 to dirs.Count - 1 do
    dirs[i].RecursiveDropNotChanged;

  for i := 0 to temp.Count - 1 do begin
    Cache:= temp[i].Value;
    Cache.ClearCache(Self, temp[i].Key);
    Cache._Release;
  end; }
end;

procedure TFileManager.DropWithSave(Dir: TDirectoryCache);
begin

end;

function TFileManager.FindObject(const FileName: string): IFileObject;
var Cache: TCacheData;
    RelativePath: string;
begin
  RelativePath:= TranslateFileName(FileName);
  Result:= nil;
  {$MESSAGE WARN 'check'}
  {if FCache.TryFileCache(RelativePath, Cache) then
    Result:= Cache.Load;}
end;

function TFileManager.GetFileLink(const FileName: string): TFileLink;
var RelativePath: string;
begin
  RelativePath:= TranslateFileName(FileName);
  Result:= TryFileLink(RelativePath, False);
end;

function TFileManager.GetFilesInDir(Directory, FileMask: string;
  List: TStrings; FileType: TFileTypes): Integer;
var ProcMask: TFileMask;

  function LikeMask(const FileName: string): Boolean;
  begin
    Result:= ProcMask.Compare(FileName);
  end;

var Attr: LongWord;

  procedure GetRealFiles(const Directory, Relative: string; List: TStrings; IsVirtual: Boolean; BeginLevel: Integer);
  var
      Search: TSearchRec;
  begin
    {$MESSAGE WARN 'проверить что все папки выбираются'}
    if FindFirst(Directory + FileMask, Attr, Search) = 0 then try
      repeat
        Search.Name:= AnsiLowerCase(Search.Name);
        if (Search.Attr and faDirectory <> 0) then begin
          if (Search.Name <> '.') and (Search.Name <> '..') then
            if (FileType = ftAllInFolder) or (FileType = ftFolders) then
              List.AddObject(Relative + Search.Name + PathDelim, TObject(BeginLevel))
            else //if (FileType = ftFilesInSubfolders) then сюда может попасть только в этом случае, т.к. иначе Attr не позволяет выбирать каталоги
              GetRealFiles(Directory + Search.Name + PathDelim, Relative + Search.Name + PathDelim, List, False, BeginLevel + 1);
        end else if FileType <> ftFolders then
          List.AddObject(Relative + Search.Name, TObject(BeginLevel));
      until FindNext(Search) <> 0;
    finally
      FindClose(Search);
    end;
  end;

  procedure GetFiles(const Directory, Relative: string; List: TStrings; IsVirtual: Boolean; BeginLevel: Integer);
  var RealPath, PathPart, tmp, checking: string;
      Search: TSearchRec;
      fc: TFolderConnect;
      i, j, first, ofs: Integer;
      addon: TLoadedAddOn;
      addonList: TLowerCaseStringList;
      af: TAddOnFile;
  begin
    for i:= FFolderConnects.Count - 1 downto 0 do begin
      fc:= FFolderConnects[i];
      if Directory.StartsWith(fc.Root) then begin
        Inc(BeginLevel);
        RealPath:= ExpandFileNameEx(fc.Folder, Copy(Directory, Length(fc.Root) + Low(string)));
        GetRealFiles(RealPath, Relative, List, True, BeginLevel);
      end else if fc.Root.StartsWith(Directory) then begin
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
    GetRealFiles(RealPath, Relative, List, True, BeginLevel);
    for i := FLoadedAddOns.Count - 1 downto 0 do begin
      addon:= FLoadedAddOns[i];
      if Directory.StartsWith(addon.Root) then begin
        addonList:= TLowerCaseStringList.Create;
        try
          addonList.CaseSensitive:= False;
          addonList.Sorted:= True;
          addon.AddOn.GetFileList(addonList); {$MESSAGE WARN 'Need check'}
          PathPart:= Copy(Directory, Length(addon.Root) + Low(string));
          addonList.Find(PathPart, first);
          for j := first to addonList.Count - 1 do
          if addonList[j].StartsWith(PathPart) then begin
            tmp:= Copy(addonList[j], Length(PathPart) + Low(string));
            ofs:= Pos(PathDelim, tmp);
            if ofs = Low(string) - 1 then begin //it is file
              if (FileType <> ftFolders) and LikeMask(tmp) then
                List.AddObject(Relative + tmp, TObject(BeginLevel));
            end else
              case FileType of
                ftAllInFolder, ftFolders: begin
                    checking:= Copy(tmp, Low(string), ofs - Low(string));
                    if LikeMask(checking) then
                      List.AddObject(Relative + checking + PathDelim, TObject(BeginLevel));
                  end;
                ftFilesInSubfolders: begin //ftFilesInSubfolders
                    checking:= Copy(tmp, tmp.LastIndexOf(PathDelim) + 1);
                    if LikeMask(checking) then
                      List.AddObject(Relative + tmp, TObject(BeginLevel));
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
              {$MESSAGE WARN 'check'}
              ofs:= Pos(PathDelim, addon.Root, Length(Directory) + Low(string));
              if ofs > 0 then
                tmp:= Copy(addon.Root, Length(Directory) + Low(string), ofs - Length(Directory) - Low(string))
              else
                tmp:= Copy(addon.Root, Length(Directory) + Low(string));
              {tmp:= '';
              for j := Length(Directory) + Low(string) to High(addon.Root) do
                if addon.Root[j] = PathDelim then begin
                  tmp:= Copy(addon.Root, Length(Directory) + Low(string), j - Length(Directory) - Low(string));
                  Break;
                end;
              if tmp = '' then
                tmp:= Copy(addon.Root, Length(Directory) + Low(string)); }
              List.AddObject(Relative + tmp + PathDelim, TObject(BeginLevel));
            end;
          ftFilesInSubfolders: begin
            addonList:= TLowerCaseStringList.Create;
            try
              addonList.CaseSensitive:= False;
              addonList.Sorted:= True;
              addon.AddOn.GetFileList(addonList);
              tmp:= Copy(addon.Root, Length(Directory) + Low(string));
              for j := 0 to addonList.Count - 1 do begin
                checking:= Copy(addonList[j], addonList[j].LastIndexOf(PathDelim) + 1);
                if LikeMask(checking) then
                  List.AddObject(Relative + tmp + addonList[j], TObject(BeginLevel));
              end;
            finally
              addonList.Free;
            end;
          end;
        end;
      end;
    end;
  end;

var Relative: string;
    Files: TStringList;
begin
  Directory:= CollapsePath(AnsiLowerCase(ReplaceStr(Directory, WrongPathDelim, PathDelim)));
  IncludeTrailingPathDelimiter(ExpandFileNameEx(FRootDirectory, AnsiLowerCase(Directory)));
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

function TFileManager.GetFileStream(Cache: TFileInfo; Options: TStreamOptions): TStream;
var s: THandleStream;
  TempPath, TempFile: string;
  BufSize: Cardinal;
  Mode: Word;
begin
  if Cache.AddOn = nil then begin
    if soNeedWrite in Options then
      Mode:= fmOpenWrite
    else
      Mode:= 0;
    Result:= TFileStream.Create(Cache.RealPath, Mode or fmOpenRead or fmShareDenyWrite);
  end else begin
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
var Cache: TFileLink;
    RelativePath: string;
begin
  RelativePath:= TranslateFileName(FileName);
  Cache:= TryFileLink(RelativePath, False);
  if Cache = nil then
    Exit(nil);
  Result:= GetFileStream(Cache.FFileInfo, Options);
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
  RelativePath:= TranslateFileName(FileName);
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
  {procedure LogFolder(Dir: TDirectoryCache);
  var i: Integer;
  begin
    for i := 0 to Dir.Count - 1 do
      if Dir.Objects[i].ClassType = TDirectoryCache then begin
        LogFolder(TDirectoryCache(Dir.Objects[i]));
      end else begin
        //Log.Add(Dir.Path + Dir[i], TCacheData(Dir.Objects[i]).RealPath);
      end;
  end; }
begin
  //LogFolder(FCache);
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
  (*procedure RecursiveSaveChanged(Dir: TDirectoryCache; OnlyChanged: TChangeStamp; const ARelativePath: string);
  var i: Integer;
      c: TFileLink;
      FileName, fd: string;
      fo: IFileObject;
  begin
    fd:= ARelativePath + Dir.Name;
    for i := 0 to Dir.FilesCount - 1 do begin
      c:= Dir.Files[i];
      {$MESSAGE WARN 'исправить'}
      fo:= nil;//c.Load;
      if(fo <> nil) and IsChangedObject(fo, OnlyChanged) then begin
        FileName:= ExpandFileNameEx(FRootDirectory, ExpandFileNameEx(ARelativePath, c.Name));
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
    end;
    for i := 0 to Dir.FoldersCount - 1 do
      RecursiveSaveChanged(Dir.Folders[i], OnlyChanged, fd);
  end; *)
begin  {$MESSAGE WARN 'check'}
  //сохранять всю папку с игрой можно только для измененных файлов
  (*if (OnlyChanged = csIgnoreChanges) and (Root = '') then
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
        RecursiveSaveChanged(Dir, OnlyChanged, '');
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
  end;  *)
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
  RelativePath:= TranslateFileName(FileName);
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

function TFileManager.TranslateFileName(const FileName: string): string;
begin
  Result:= CollapsePath(AnsiLowerCase(ReplaceStr(FileName, WrongPathDelim, PathDelim)));
  if not IsRelativePath(Result) then
    Result:= ExtractRelativePath(FRootDirectory, Result);
end;

function TFileManager.TryCacheNote(const RelativePath: string; CreateNew: Boolean): TCacheData;
  {function GetAddOnCache(const RelativePath, RealPath: string; IsVirtual: Boolean): TCacheData;
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
  end;}
var FullDir, RealPath, RealPathForCreate: string;
    fc: TFolderConnect;
    i: Integer;
begin
  (*if not FCache.TryFileCache(RelativePath, Result) then begin
    FullDir:= ExpandFileNameEx(FRootDirectory, RelativePath);
    RealPathForCreate:= FullDir;
    //проверяем все подключенные каталоги и моды в них последние подключенные имеют более высокий приоритет
    for i := FFolderConnects.Count - 1 downto 0 do begin
      fc:= FFolderConnects[i];
      if FullDir.StartsWith(fc.Root) then begin //это виртуальный путь
        RealPath:= ExpandFileNameEx(fc.Folder, Copy(FullDir, Length(fc.Root) + 1));
        if FileExists(RealPath) then
          Exit(AddCacheNote(RealPath, RelativePath, nil, True));
        Result:= GetAddOnCache(RelativePath, RealPath, True);
        if Result <> nil then
          Exit
        else if CreateNew and (RealPathForCreate = FullDir) then //если нужно создать файл и он попадает в подключенный каталог, то запомним его реальное место с учётом перенаправлений
          RealPathForCreate:= RealPath;
      end;
    end;
    {$MESSAGE WARN 'Создание файлов в каталоге аддона создаёт реальный файл'}
    if FileExists(FullDir) then
      Result:= AddCacheNote(FullDir, RelativePath, nil, False)
    else
      Result:= GetAddOnCache(RelativePath, FullDir, False);
    if (Result = nil) and CreateNew then
      Exit(AddCacheNote(RealPathForCreate, RelativePath, nil, RealPathForCreate <> FullDir));
  end; *)
end;

function TFileManager.TryFileLink(const RelativePath: string; CreateNew: Boolean): TFileLink;
  function GetAddOnCache(const RelativePath, RealPath: string; IsVirtual: Boolean): TFileLink;
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
          Result:= AddFileLink(RealPath, RelativePath, @af, IsVirtual);
          Exit;
        end;
      end;
    end;
    Result:= nil;
  end;
var FullDir, RealPath, RealPathForCreate: string;
    fc: TFolderConnect;
    i: Integer;
begin
  if not FCache.TryFileLink(RelativePath, Result) then begin
    FullDir:= ExpandFileNameEx(FRootDirectory, RelativePath);
    RealPathForCreate:= FullDir;
    //проверяем все подключенные каталоги и моды в них последние подключенные имеют более высокий приоритет
    for i := FFolderConnects.Count - 1 downto 0 do begin
      fc:= FFolderConnects[i];
      if FullDir.StartsWith(fc.Root) then begin //это виртуальный путь
        RealPath:= ExpandFileNameEx(fc.Folder, Copy(FullDir, Length(fc.Root) + 1));
        if FileExists(RealPath) then
          Exit(AddFileLink(RealPath, RelativePath, nil, True));
        if CreateNew and (RealPathForCreate = FullDir) then //если нужно создать файл и он попадает в подключенный каталог, то запомним его реальное место с учётом перенаправлений
          RealPathForCreate:= RealPath;
      end;
    end;
    {$MESSAGE WARN 'Создание файлов в каталоге аддона создаёт реальный файл'}
    if FileExists(FullDir) then
      Result:= AddFileLink(FullDir, RelativePath, nil, False)
    else
      Result:= GetAddOnCache(RelativePath, FullDir, False);
    if (Result = nil) and CreateNew then
      Exit(AddFileLink(RealPathForCreate, RelativePath, nil, RealPathForCreate <> FullDir));
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

procedure TDirectoryCache.AddFileLink(const Name: string; Cache: TFileLink);
var Dir, b: TDirectoryCache;
    Dirs: TStringDynArray;
    i, j, k: Integer;
begin
  Dirs:= SplitString(Name, PathDelim);
  Dir:= Self;

  for i := 0 to High(Dirs) - 1 do begin
    b:= Dir;
    MonitorEnter(b);
    try
      if not Dir.FindFolder(Dirs[i], j) then begin
        if Dir.FindFile(Dirs[i], k) then begin
          DebugBreak;
          raise Exception.Create('Есть файл с названием: ' + Dir.Name + PathDelim + Dirs[i] + '. Нельзя создать папку с таким же названием.');
        end;
        Dir.FFolders.Insert(j, TDirectoryCache.Create({Dir.Path + }Dirs[i]{ + PathDelim}));
      end;
      Dir:= Dir.FFolders[j];
    finally
      MonitorExit(b);
    end;
  end;

  MonitorEnter(Dir);
  try
    if Dirs = nil then
      Cache.FName:= Name
    else
      Cache.FName:= Dirs[High(Dirs)];
    if Dir.FindFile(Cache.Name, j) then
      raise Exception.Create('Уже есть файл с названием: ' + Name)
    else if Dir.FindFolder(Cache.Name, k) then begin
      DebugBreak;
      raise Exception.Create('Есть каталог с названием: ' + Name + '. Нельзя создать файл с таким же названием.');
    end;
    Dir.FFiles.Insert(j, Cache);
  finally
    MonitorExit(Dir);
  end;
end;

procedure TDirectoryCache.Clear(DoSave: Boolean);
var i, Index: Integer;
    Cache: TFileLink;
    fo: IFileObject;
    files: TArray<TFileLink>;
    dirs: TArray<TDirectoryCache>;
    Stream: TStream;
  Instance: TCacheInstance;
  tmp: TCacheInstance;
  good: Boolean;
begin
  MonitorEnter(Self);
  try
    files:= Copy(FFiles.List, 0, FFiles.Count);
    FFiles.Clear;
    dirs:= Copy(FFolders.List, 0, FFolders.Count);
    FFolders.Clear;
  finally
    MonitorExit(Self);
  end;

  for i := 0 to High(dirs) do begin
    dirs[i].Clear(DoSave);
    dirs[i].Destroy;
  end;

  for i := 0 to High(files) do begin
    Cache:= files[i];
    {$MESSAGE WARN 'fix it'}
    (*if DoSave then begin
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
    Cache.ClearCache(Self, temp[i].Key); *)
    Cache.Destroy;
  end;
end;

class constructor TDirectoryCache.Create;
begin
  FDirectoryComparer:= TNamedObjectComparer.Create;
end;

constructor TDirectoryCache.Create(const APath: string);
begin
  inherited Create;
  FFolders.Create(IComparer<TDirectoryCache>(DirectoryComparer), 4);
  FFiles.Create(IComparer<TFileLink>(DirectoryComparer), 4);
  FName:= APath;
end;

destructor TDirectoryCache.Destroy;
begin
  Clear(False);
  inherited;
end;

function TDirectoryCache.FindFile(const S: string; var Index: Integer): Boolean;
var item: TNamedObjectRec;
begin
  item.Name:= s;
  Result:= TArray.BinarySearch<TFileLink>(FFiles.List, TFileLink(@item), Index, FFiles.Comparer, 0, FFiles.Count);
end;

function TDirectoryCache.FindFolder(const S: string; var Index: Integer): Boolean;
var item: TNamedObjectRec;
begin
  item.Name:= s;
  Result:= TArray.BinarySearch<TDirectoryCache>(FFolders.List, TDirectoryCache(@item), Index, FFolders.Comparer, 0, FFolders.Count);
end;

function TDirectoryCache.GetDirectory(const Name: string): TDirectoryCache;
var i, j: Integer;
    Dirs: TStringDynArray;
begin
  if Name = '' then
    Exit(Self);

  Dirs:= SplitString(Name, PathDelim);

  if Name[High(Name)] = PathDelim then
    SetLength(Dirs, Length(Dirs) - 1);

  Result:= GetDirectory(Dirs);
end;

function TDirectoryCache.GetDirectory(const List: array of string): TDirectoryCache;
var i, j: Integer;
    b: TDirectoryCache;
begin
  Result:= Self;

  for i := 0 to High(List) do begin
    b:= Result;
    try
      MonitorEnter(b);
      if Result.FindFolder(List[i], j) then
        Result:= Result.FFolders[j]
      else
        Exit(nil);
    finally
      MonitorExit(b);
    end;
  end;
end;

function TDirectoryCache.GetDirectoryCache(AIndex: Integer): TDirectoryCache;
begin
  Result:= FFolders[AIndex];
end;

function TDirectoryCache.GetFileCache(const Name: string): TFileLink;
type
  TStringArray = array [0..0] of string;
  PStringArray = ^TStringArray;
var Dir: TDirectoryCache;
    p: Integer;
    FileName: string;
    FullName: string;
    Dirs: TStringDynArray;
begin
  if Name = '' then
    Exit(nil);

  Dirs:= SplitString(Name, PathDelim);

  p:= LastDelimiter(PathDelim, Name);
  if Length(Dirs) > 1 then begin
    {$RANGECHECKS OFF}
    Dir:= GetDirectory(Slice(PStringArray(Dirs)^, High(Dirs)));
    {$IFDEF DEBUG}{$RANGECHECKS ON}{$ENDIF}
    if Dir = nil then
      Exit(nil);
  end else
    Dir:= Self;
  FileName:= Dirs[High(Dirs)];

  MonitorEnter(Dir);
  try
    if Dir.FindFile(FileName, p) then
      Exit(Dir.FFiles[p]);
  finally
    MonitorExit(Dir);
  end;
  Result:= nil;
end;

function TDirectoryCache.GetFileLink(AIndex: Integer): TFileLink;
begin
  Result:= FFiles[AIndex];
end;

function TDirectoryCache.GetFilesCount: Integer;
begin
  Result:= FFiles.Count;
end;

function TDirectoryCache.GetFoldersCount: Integer;
begin
  Result:= FFolders.Count;
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
  {$MESSAGE WARN 'fix it'}
  {MonitorEnter(Self);
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
  end; }
end;

function TDirectoryCache.TryFileLink(const Name: string; var Cache: TFileLink): Boolean;
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
  {if MonitorTryEnter(Self) then
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
  end; }
end;

constructor TCacheData.Create(AManager: TFileManager);
begin
  FManager:= AManager;
end;

function TCacheData.CreateStream: TStream;
begin
  {$MESSAGE WARN 'check'}
  //Result:= FManager.GetFileStream(Self, Options);
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

{ TNamedObjectComparer }

function TNamedObjectComparer.Compare(const Left, Right: TNamedObject): Integer;
begin
  Result := AnsiCompareStr(Left.Name, Right.Name);
end;

{ TDelegatedInterface }

constructor TDelegatedInterface.Create(AOwner: TObject);
begin
  FOwner:= AOwner;
end;

destructor TDelegatedInterface.Destroy;
begin
  if (FOwner <> Self) and (FOwner <> nil) then
    FOwner.Destroy;
  inherited;
end;

function TDelegatedInterface.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if FOwner.GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ TFileLink }

constructor TFileLink.Create(AFileInfo: TFileInfo);
begin
  FFileInfo:= AFileInfo;
end;

procedure TFileLink.DoUpdateFile;
begin

end;

function TFileLink.GetFileStream(Options: TStreamOptions): TStream;
begin

end;

function TFileLink.HasSubscribers: Boolean;
begin
  Result:= True;
end;

{ TFileInfo }

constructor TFileInfo.Create(const AAddOn: IAddOn; const ARealPath: string);
begin
  FAddOn:= AAddOn;
  FRealPath:= ARealPath;
end;

function TFileInfo.Equals(Obj: TObject): Boolean;
begin
  if (Self = nil) or (Obj = nil) then
    Result:= Self = Obj
  else
    Result:= (Self.FAddOn = TFileInfo(Obj).FAddOn) and (Self.FRealPath = TFileInfo(Obj).FRealPath);
end;

function TFileInfo.GetHashCode: Integer;
begin
  Result:= BobJenkinsHash(FRealPath[Low(string)], Length(FRealPath) * SizeOf(Char), 0)
    xor BobJenkinsHash(Pointer(FAddOn), SizeOf(Pointer), 0);
end;

end.
