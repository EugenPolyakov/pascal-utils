unit GlobalMap;

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Collections,
     Vcl.Graphics, Vcl.Imaging.pngimage, RecordUtils;

type
  TBMHeader = packed record
    Sign: array [0..3] of AnsiChar;
    Width, Height: Integer;
    bpp, Version: Byte;
  end;

  TBMExtended = packed record
    ProcessedBPP: Byte;
    IsRLE: ByteBool;
    AreasCount: Integer;
  end;

  TDirection = (dRight, dBottom, dLeft, dUp);

  TGlobalMapBase = class;

  TPathAreaCallback = reference to function (Map: TGlobalMapBase; CurrX, CurrY: Integer; Direct: TDirection): Boolean;

  TBacklightBase = class
  private
  strict protected
    FX, FY: Integer;
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
  public
    property X: Integer read FX;
    property Y: Integer read FY;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    constructor Create; virtual;
  end;

  TBacklightPngBase = class (TBacklightBase)
  private
  strict protected
    FBacklight: TPngImage;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
  public
    property Backlight: TPngImage read FBacklight;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(Output: TCanvas; AOffset: TPoint);
  end;

  TBacklightAuto = class (TBacklightPngBase)
  strict protected
    FArea: Integer;
  public
    procedure BuildArea(Map: TGlobalMapBase; Area: Integer; AColor: TColor); virtual; abstract;
    property Area: Integer read FArea;
  end;

  TBacklightAutoGenerator = function : TBacklightAuto of object;

  TBacklight = class (TBacklightAuto)
  private
    FColor: TColor;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BuildArea(Map: TGlobalMapBase; Area: Integer; AColor: TColor); override;
  end;

  TAreaInfo = record
    Area: Integer;
    Color: TColor;
  end;

  TAreaSizeType = (astInner, astOuter, astOuterWithUnbound);

  TAreaSizeCalculator = record
  private
    FGM: TGlobalMapBase;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
  public
    Left, Top, Right, Bottom: Longint;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    procedure Initialize(m: TGlobalMapBase);
    procedure CalculateAreaSize(X, Y: Integer; SizeType: TAreaSizeType); overload;
    procedure CalculateAreaSize(Area: Integer; SizeType: TAreaSizeType); overload;
  end;

  TBacklightPngWriter = record
  private
    FBorderAlpha: Byte;
    FFillAlpha: Byte;
    FRGB: TRGBTriple;
    FCurrentColor: TColor;
    procedure SetBorderAlpha(Value: Byte);
    procedure SetFillAlpha(Value: Byte);
    procedure SetCurrentColor(const Value: TColor);
    procedure DebugCheck(cX, cY: Integer);
  public
    Backlight: TPngImage;
    X, Y: Integer;
    property CurrentColor: TColor read FCurrentColor write SetCurrentColor;
    property BorderAlpha: Byte read FBorderAlpha write SetBorderAlpha;
    property FillAlpha: Byte read FFillAlpha write SetFillAlpha;
    function DoBorder(m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean;
    function DoPrepareToFill(m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean;
    function DoFastFill(m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean;
    procedure DoFill;
  end;

  TGetExtendedAreaList = function (ParentArea: Integer): TArray<TAreaInfo> of object;

  TExtendedBacklight = class (TBacklightAuto)
  private
    FBorderAlpha: Byte;
    FFillAlpha: Byte;
    FGetExtendedAreaList: TGetExtendedAreaList;
    procedure SetBorderAlpha(Value: Byte);
    procedure SetFillAlpha(Value: Byte);
  protected
  public
    FillOnlyMainArea: Boolean;
    property BorderAlpha: Byte read FBorderAlpha write SetBorderAlpha;
    property FillAlpha: Byte read FFillAlpha write SetFillAlpha;
    constructor Create; override;
    property GetExtendedAreaList: TGetExtendedAreaList read FGetExtendedAreaList write FGetExtendedAreaList;
    procedure BuildArea(Map: TGlobalMapBase; Area: Integer; AColor: TColor); override;
  end;

  TBacklightArrayWriter = record
  private
    FBorderAlpha: Byte;
    FFillAlpha: Byte;
    procedure SetBorderAlpha(Value: Byte);
    procedure SetFillAlpha(Value: Byte);
  public
    Backlight: array of TColor;
    CurrentColor: TColor;
    X, Y, Width, Height: Integer;
    property BorderAlpha: Byte read FBorderAlpha write SetBorderAlpha;
    property FillAlpha: Byte read FFillAlpha write SetFillAlpha;
    function DoBorder(m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean;
    function DoPrepareToFill(m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean;
    procedure DoFill;
  end;

  TScaleGradation = (sg500, sg625, sg750, sg875, sg1000);
  //(sg1to2 {50%}, sg2to3 {66%}, sg3to4 {75%}, sg9to10 {90%}, sg1to1);

  TScrollGraphic = class
  private
    FCurrX, FCurrY, FDragX, FDragY: Integer;
    FUseDrag, FIsDrag: Boolean;
    FOutWidth, FOutHeight: Integer;
    procedure SetOutHeight(const Value: Integer);
    procedure SetOutWidth(const Value: Integer);
  protected
    FWidth, FHeight: Integer;
    FScale: TScaleGradation;
    procedure SetScale(Value: TScaleGradation); virtual;
    procedure CorrectCurrX;
    procedure CorrectCurrY;
  public
    property CurrX: Integer read FCurrX;
    property CurrY: Integer read FCurrY;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    function ConverToDefaultScale(Value: Integer): Integer;
    function ConvertToCurrentScale(Value: Integer): Integer;
    property Scale: TScaleGradation read FScale write SetScale;
    //Размеры видимой области
    property OutWidth: Integer read FOutWidth write SetOutWidth;
    property OutHeight: Integer read FOutHeight write SetOutHeight;
    property IsDrag: Boolean read FIsDrag;//Было ли перетаскивание
    property UseDrag: Boolean read FUseDrag;//Происходит ли перетаскивание
    procedure UpdateImage; virtual; abstract;
    function ConvertToReal(X, Y: Integer): TPoint;
    procedure StartDrag(X, Y: Integer);//Начало перетаскивания
    procedure StopDrag;//Конец перетаскивания карты
    procedure AutoDrag(X, Y: Integer; Update: Boolean);//Перерисовка карты во время перетаскивания
    constructor Create;
  end;

  TGlobalMapBase = class (TScrollGraphic)
  private
  protected
    FAutoBacklight: Integer;
    FBacklightList: TListRecord<TBacklightBase>;
    FUniqueObjects: TDictionary<string, TObject>;
    FBytesPerPixel: Byte;
    FProcessedBytesPerArea: Integer;
    FAreasCount: Integer;
    FProcessedData: PAnsiChar;
    FBitMap: PAnsiChar;  //Почти свой формат))
    FOffsetX: Integer;
    FOffsetY: Integer;
    function GetScaleHeight: Integer; virtual; abstract;
    function GetScaleWidth: Integer; virtual; abstract;
    procedure SetAreaCenter(Area: Integer; X, Y: Integer);
    function GetCustomBacklight(Index: Integer): TBacklightBase;
    function GetCustomBacklightCount: Integer; inline;
    procedure SetCustomBacklight(Index: Integer; const Value: TBacklightBase);
    procedure LoadAreaInfo(ABitMap: TStream);
    procedure RemoveObjects(Sender: TObject; const Item: TObject; Action: TCollectionNotification); virtual;
  public
    property ScaleWidth: Integer read GetScaleWidth;
    property ScaleHeight: Integer read GetScaleHeight;
    property OutX: Integer read FOffsetX write FOffsetX;
    property OutY: Integer read FOffsetY write FOffsetY;
    property CustomBacklightCount: Integer read GetCustomBacklightCount;
    function AddCustomBacklight(bl: TBacklightBase): Integer;
    function ExtractCustomBacklight(Index: Integer): TBacklightBase;
    procedure DeleteCustomBacklight(Index: Integer); virtual;
    property CustomBacklight[Index: Integer]:TBacklightBase read GetCustomBacklight write SetCustomBacklight;
    constructor Create;
    procedure Load(const AMap, ABitMap: string; AOutWidth, AOutHeight: Integer); overload;
    procedure Load(AMap: TGraphic; ABitMap: TStream; AOutWidth, AOutHeight: Integer); overload; virtual; abstract;
    destructor Destroy; override;
    function GetAreaAt(Pos: TPoint): Integer;//Получить ID области в координатах карты
    procedure SetBacklight(Pos: TPoint; Color: TColor); virtual; abstract;//подсветить область под указанными координатам
    procedure RecreateProcessedData;
    property AreasCount: Integer read FAreasCount;
    function GetAreaCenter(Area: Integer): TPoint;
    //Добавить объект поверх карты
    //ID - переводится в верхний регистр
    //если добавть обект, который уже есть, то старый удалится и добавится новый
    procedure SetUniqueObject(X, Y: Integer; const ID: string; AObject: TGraphic); virtual; abstract;
    //Удалить объект
    procedure DeleteUniqueObject(const ID: string); virtual; abstract;
    procedure RefreshScales; virtual;
    procedure PathArea(X, Y: Integer; CoordInDefaultScale, UseAround: Boolean; Callback: TPathAreaCallback); overload;
    procedure PathArea(Area: Integer; UseAround: Boolean; Callback: TPathAreaCallback); overload;
  end;

  TGlobalMap = class (TGlobalMapBase)
  private
    //Все имеющиеся косяки реализации сейчас нет смысла править,
    //в будущем при их исправлении интерфейс меняться не будет,
    //а для их исправления нужны сложные структуры данных, которые на данном этапе слишком накладно делать
    FMap: array [TScaleGradation] of TBitmap; //Для очень больших карт простой bmp тоже не катит нужно сделать кеширование
    FBacklightGenerator: TBacklightAutoGenerator;
    FOutput: TCanvas;
    procedure SetMap(const Value: TBitmap);
    function GetMap: TBitmap;
  protected
    function GetScaleHeight: Integer; override;
    function GetScaleWidth: Integer; override;
    procedure SetScale(Value: TScaleGradation); override;
    property Map: TBitmap read GetMap write SetMap;
    procedure ShowBacklight(Output: TCanvas; AOffset: TPoint);
  public
    property BacklightGenerator: TBacklightAutoGenerator read FBacklightGenerator write FBacklightGenerator;
    constructor Create(AOutput: TCanvas);
    procedure Load(AMap: TGraphic; ABitMap: TStream; AOutWidth, AOutHeight: Integer); overload; override;
    destructor Destroy; override;
    procedure StopBacklight;
    procedure UpdateImage; overload; override;//Обновить изображение карты
    procedure SetBacklight(Pos: TPoint; Color: TColor); override;//подсветить область под указанными координатам
    property AreasCount: Integer read FAreasCount;
    //Добавить объект поверх карты
    //ID - переводится в верхний регистр
    //если добавть обект, который уже есть, то старый удалится и добавится новый
    procedure SetUniqueObject(X, Y: Integer; const ID: string; AObject: TGraphic); override;
    //Удалить объект
    procedure DeleteUniqueObject(const ID: string); override;
    procedure RefreshScales; override;
  end;

  TUniqueObject = class (TBitmap)
  private
  protected
  public
    X, Y: Integer;
    constructor Create(AX, AY, AWidth, AHeight: Integer); reintroduce;
  end;

const
  CurrentVersion = 2;
  Offsets: array [TDirection] of Integer = (1, 1, -1, -1);

procedure MoveByDirection(var X, Y: Integer; Direction: TDirection); overload; inline;
function RotateLeft(Direction: TDirection): TDirection; inline;
function RotateRight(Direction: TDirection): TDirection; inline;

implementation

uses Math, System.Types, SysTypes;

procedure MoveByDirection(var X, Y: Integer; Direction: TDirection);
begin
  if Ord(Direction) mod 2 = 0 then
    Inc(X, 1 - Ord(Direction) and 2)
  else
    Inc(Y, 1 - Ord(Direction) and 2);
end;

function RotateLeft(Direction: TDirection): TDirection;
begin
  Result:= TDirection((Ord(Direction) + 4 - 1) mod 4);
end;

function RotateRight(Direction: TDirection): TDirection;
begin
  Result:= TDirection((Ord(Direction) + 1) mod 4);
end;

{ TScrollGraphic }

procedure TScrollGraphic.AutoDrag(X, Y: Integer; Update: Boolean);
begin
  if not FUseDrag then Exit;
  Inc(FCurrX, ConverToDefaultScale(FDragX - X));
  CorrectCurrX;
  Inc(FCurrY, ConverToDefaultScale(FDragY - Y));
  CorrectCurrY;
  FIsDrag:= (FDragX <> X) or (FDragY <> Y);
  if FIsDrag and Update then
    UpdateImage;
  FDragX:= X;
  FDragY:= Y;
end;

function TScrollGraphic.ConvertToReal(X, Y: Integer): TPoint;
begin
  Result.X:= ConverToDefaultScale(X) + FCurrX;
  Result.Y:= ConverToDefaultScale(Y) + FCurrY;
end;

procedure TScrollGraphic.CorrectCurrX;
begin
  if (FCurrX < 0) or (Width <= ConverToDefaultScale(FOutWidth)) then
    FCurrX:= 0
  else if FCurrX > Width - ConverToDefaultScale(FOutWidth) then
    FCurrX:= Width - ConverToDefaultScale(FOutWidth);
end;

procedure TScrollGraphic.CorrectCurrY;
begin
  if (FCurrY < 0) or (Height <= ConverToDefaultScale(FOutHeight)) then
    FCurrY:= 0
  else if FCurrY > Height - ConverToDefaultScale(FOutHeight) then
    FCurrY:= Height - ConverToDefaultScale(FOutHeight);
end;

constructor TScrollGraphic.Create;
begin
  FScale:= sg1000;//sg1to1;
end;

function TScrollGraphic.ConvertToCurrentScale(Value: Integer): Integer;
begin
  Result:= Value;
  case FScale of
    sg500: Result:= Round(Value / 2);
    sg625: Result:= Round(Value * 5 / 8);
    sg750: Result:= Round(Value * 3 / 4);
    sg875: Result:= Round(Value * 7 / 8);
    {sg1to2: Result:= Round(Value / 2);
    sg2to3: Result:= Round(Value * 2 / 3);
    sg3to4: Result:= Round(Value * 3 / 4);
    sg9to10: Result:= Round(Value * 9 / 10); }
  end;
end;

function TScrollGraphic.ConverToDefaultScale(Value: Integer): Integer;
begin
  Result:= Value;
  case FScale of
    sg500: Result:= Round(Value * 2);
    sg625: Result:= Round(Value * 8 / 5);
    sg750: Result:= Round(Value * 4 / 3);
    sg875: Result:= Round(Value * 8 / 7);
    {sg1to2: Result:= Round(Value * 2);
    sg2to3: Result:= Round(Value * 3 / 2);
    sg3to4: Result:= Round(Value * 4 / 3);
    sg9to10: Result:= Round(Value * 10 / 9); }
  end;
end;

procedure TScrollGraphic.SetOutHeight(const Value: Integer);
begin
  FOutHeight := Value;
  CorrectCurrY;
end;

procedure TScrollGraphic.SetOutWidth(const Value: Integer);
begin
  FOutWidth := Value;
  CorrectCurrX;
end;

procedure TScrollGraphic.SetScale(Value: TScaleGradation);
var oldX, oldY: Integer;
begin
  oldX:= ConverToDefaultScale(FOutWidth div 2);
  oldY:= ConverToDefaultScale(FOutHeight div 2);
  FScale:= Value;
  StopDrag;
  if Width > 0 then begin
    FCurrX:= FCurrX + oldX - ConverToDefaultScale(FOutWidth div 2);
    FCurrY:= FCurrY + oldY - ConverToDefaultScale(FOutHeight div 2);
    CorrectCurrX;
    CorrectCurrY;
  end;
end;

procedure TScrollGraphic.StartDrag(X, Y: Integer);
begin
  FDragX:= X;
  FDragY:= Y;
  FUseDrag:= True;
end;

procedure TScrollGraphic.StopDrag;
begin
  FUseDrag:= False;
  FIsDrag:= False;
end;

{ TGlobalMap }

constructor TGlobalMap.Create(AOutput: TCanvas);
begin
  inherited Create;
  FOutput:= AOutput;
  FMap[sg1000]:= TBitmap.Create;
end;

procedure TGlobalMap.DeleteUniqueObject(const ID: string);
var s: string;
    b: TUniqueObject;
begin
  s:= UpperCase(ID);
  if FUniqueObjects.TryGetValue(s, TObject(b)) then begin
    //восстанавливаем старую часть карты
    FMap[sg1000].Canvas.CopyRect(Rect(b.X, b.Y, b.X + b.Width, b.Y + b.Height),
        b.Canvas, Rect(0, 0, b.Width, b.Height));
    FUniqueObjects.Remove(s);
    b.Free;
  end;
end;

destructor TGlobalMap.Destroy;
var
  i: TScaleGradation;
begin
  for i := Low(TScaleGradation) to High(TScaleGradation) do
    FMap[i].Free;

  inherited;
end;

function TGlobalMap.GetMap: TBitmap;
begin
  Result:= FMap[sg1000];
end;

function TGlobalMap.GetScaleHeight: Integer;
begin
  Result:= FMap[Scale].Height;
end;

function TGlobalMap.GetScaleWidth: Integer;
begin
  Result:= FMap[Scale].Width;
end;

procedure TGlobalMap.Load(AMap: TGraphic; ABitMap: TStream; AOutWidth,
  AOutHeight: Integer);
begin
  FMap[sg1000].Height:= AMap.Height;
  FMap[sg1000].Width:= AMap.Width;
  FHeight:= FMap[sg1000].Height;
  FWidth:= FMap[sg1000].Width;

  LoadAreaInfo(ABitMap);

  FMap[sg1000].Canvas.Draw(0, 0, AMap);
  OutWidth:= AOutWidth;
  OutHeight:= AOutHeight;

  RefreshScales;
end;

procedure TGlobalMap.RefreshScales;
begin
  FreeAndNil(FMap[sg500]);
  FreeAndNil(FMap[sg625]);
  FreeAndNil(FMap[sg750]);
  FreeAndNil(FMap[sg875]);

  SetScale(Scale);
end;

procedure TGlobalMap.SetBacklight(Pos: TPoint; Color: TColor);
var area: Integer;
    b: TBacklightAuto;
begin
  area:= GetAreaAt(Pos);
  if area = 0 then begin
    StopBacklight;
    Exit;
  end;

  if FAutoBacklight >= 0 then begin
    if TBacklightAuto(FBacklightList[FAutoBacklight]).Area = area then
      Exit
    else
      StopBacklight;
  end;

  if Assigned(FBacklightGenerator) then
    b:= FBacklightGenerator
  else
    b:= TBacklight.Create;
  FAutoBacklight:= FBacklightList.Add(b);
  b.BuildArea(Self, area, Color);
end;

procedure TGlobalMap.SetMap(const Value: TBitmap);
begin
  FMap[sg1000]:= Value;
end;

procedure TGlobalMap.SetScale(Value: TScaleGradation);
begin
  inherited;
  if FMap[Value] = nil then begin
    FMap[Value]:= TBitmap.Create;
    FMap[Value].SetSize(ConvertToCurrentScale(Width), ConvertToCurrentScale(Height));
    SetStretchBltMode(FMap[Value].Canvas.Handle, {STRETCH_}HALFTONE);

    FMap[Value].Canvas.CopyRect(Rect(0,0, FMap[Value].Width, FMap[Value].Height), FMap[sg1000].Canvas,
      Rect(0, 0, FMap[sg1000].Width, FMap[sg1000].Height));
  end;
end;

procedure TGlobalMap.SetUniqueObject(X, Y: Integer; const ID: string;
  AObject: TGraphic);
var b: TUniqueObject;
    s: string;
begin
  s:= UpperCase(ID);
  //На данный момент - так, возможно стоит переделать
  //при таком подходе могут быть косяки если несколько объектов накладываются друг на друга
  //в таком случае если добавить объекты в одном порядке, а удалять в другом, то возможны косяки
  //- после удаления объекта могут остаться части уже удаленного другого объекта
  if FUniqueObjects.TryGetValue(s, TObject(b)) then begin
    //восстанавливаем старую часть карты
    FMap[sg1000].Canvas.CopyRect(Rect(b.X, b.Y, b.X + b.Width, b.Y + b.Height),
        b.Canvas, Rect(0, 0, b.Width, b.Height));
    b.X:= X;
    b.Y:= Y;
    b.Width:= AObject.Width;
    b.Height:= AObject.Height;
  end else begin
    b:= TUniqueObject.Create(X, Y, AObject.Width, AObject.Height);
    FUniqueObjects.Add(s, b);
  end;
  //копируем часть карты, которая будет закрыта объектом
  b.Canvas.CopyRect(Rect(0, 0, AObject.Width, AObject.Height),
      FMap[sg1000].Canvas, Rect(X, Y, X + AObject.Width, Y + AObject.Height));
  //рисуем объект на карте
  FMap[sg1000].Canvas.Draw(X, Y, AObject);
end;

procedure TGlobalMap.ShowBacklight(Output: TCanvas; AOffset: TPoint);
var
  i: Integer;
begin
  for i := 0 to FBacklightList.Count - 1 do
    TBacklightPngBase(FBacklightList[i]).Draw(Output, Point(AOffset.X - ConvertToCurrentScale(CurrX),
        AOffset.Y - ConvertToCurrentScale(CurrY)));
end;

procedure TGlobalMap.StopBacklight;
begin
  if FAutoBacklight >= 0 then begin
    FBacklightList[FAutoBacklight].Free;
    FBacklightList.Delete(FAutoBacklight);
    FAutoBacklight:= -1;
  end;
end;

procedure TGlobalMap.UpdateImage;
var r: TRect;
begin
  if FMap[Scale] = nil then Exit;
  r.Create(0, 0, OutWidth, OutHeight);
  r.Offset(OutX, OutY);
  FOutput.CopyRect(r, FMap[Scale].Canvas,
    Rect(ConvertToCurrentScale(CurrX), ConvertToCurrentScale(CurrY), OutWidth + ConvertToCurrentScale(CurrX), OutHeight + ConvertToCurrentScale(CurrY)));

  ShowBacklight(FOutput, Point(OutX, OutY));
end;

{ TUniqueObject }

constructor TUniqueObject.Create(AX, AY, AWidth, AHeight: Integer);
begin
  inherited Create;
  SetSize(AWidth, AHeight);
  X:= AX;
  Y:= AY;
end;

{ TBacklight }

constructor TBacklight.Create;
begin
  inherited;
end;

procedure TBacklight.BuildArea(Map: TGlobalMapBase; Area: Integer; AColor: TColor);
var r: TAreaSizeCalculator;
begin
  FColor:= AColor;
  FBacklight.Free;
  FBacklight:= nil;

  //вычисляем размеры
  r.Initialize(Map);
  r.CalculateAreaSize(Area, astOuter);
  FX:= r.Left;
  FY:= r.Top;

  FBacklight:= TPngImage.CreateBlank(COLOR_RGBALPHA, 8, r.Width + 1, r.Height + 1);

  //зарисовываем
  Map.PathArea(Area, False, function (m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean
    begin
      FBacklight.Pixels[cX - FX, cY - FY]:= FColor;
      FBacklight.AlphaScanline[cY - FY][cX - FX]:= $ff;
      Result:= True;
    end);
end;

destructor TBacklight.Destroy;
begin
  inherited;
end;

{ TBacklightBase }

constructor TBacklightBase.Create;
begin
  inherited;
end;

{ TExtendedBacklight }

procedure TExtendedBacklight.BuildArea(Map: TGlobalMapBase; Area: Integer;
  AColor: TColor);
var exAreas: TArray<TAreaInfo>;
  i: Integer;
  r: TAreaSizeCalculator;
  w: TBacklightPngWriter;
begin
  FBacklight.Free;
  FBacklight:= nil;

  //вычисляем размеры
  r.Initialize(Map);
  r.CalculateAreaSize(Area, astOuterWithUnbound);

  exAreas:= GetExtendedAreaList(Area);

  for i := 0 to High(exAreas) do
    r.CalculateAreaSize(exAreas[i].Area, astOuterWithUnbound);

  FX:= r.Left;
  FY:= r.Top;

  FBacklight:= TPngImage.CreateBlank(COLOR_RGBALPHA, 8, r.Width + 1, r.Height + 1);
  w.Backlight:= FBacklight;
  w.X:= FX;
  w.Y:= FY;
  w.BorderAlpha:= BorderAlpha;
  w.FillAlpha:= FillAlpha;

  //обводим оригинал
  w.CurrentColor:= AColor;
  Map.PathArea(Area, True, w.DoPrepareToFill);

  //зарисовываем оригинал
  if FillOnlyMainArea then
    if FillAlpha > 0 then
      w.DoFill;

  //обводим соседние
  for i := 0 to High(exAreas) do begin
    w.CurrentColor:= exAreas[i].Color;
    Map.PathArea(exAreas[i].Area, True, w.DoPrepareToFill);
  end;

  //зарисовываем соседние
  if not FillOnlyMainArea then
    if FillAlpha > 0 then
      w.DoFill;
end;

constructor TExtendedBacklight.Create;
begin
  inherited;
  FBorderAlpha:= $88;
  FFillAlpha:= $22;
end;

procedure TExtendedBacklight.SetBorderAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value)
  else if Value = 0 then
    Inc(Value);
  if Value = FFillAlpha then
    if Value = 1 then
      Inc(Value)
    else
      Dec(Value);
  FBorderAlpha := Value;
end;

procedure TExtendedBacklight.SetFillAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value);
  if Value = FBorderAlpha then
    if Value = 0 then
      Inc(Value)
    else
      Dec(Value);
  FFillAlpha := Value;
end;

{ TAreaSizeCalculator }

procedure TAreaSizeCalculator.CalculateAreaSize(X, Y: Integer;
  SizeType: TAreaSizeType);
var L, T, B, R: Integer;
begin
  L:= Left;
  R:= Right;
  T:= Top;
  B:= Bottom;
  //вычисляем размеры
  FGM.PathArea(X, Y, True, SizeType = astOuterWithUnbound, function (m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean
    var x, y: Integer;
    begin
      x:= cX;
      y:= cY;
      if SizeType = astInner then
        MoveByDirection(x, y, RotateRight(Direct));
      if X > R then
        R:= X;
      if Y > B then
        B:= Y;
      if X < L then
        L:= X;
      if Y < T then
        T:= Y;
      Result:= True;
    end);
  Left:= L;
  Right:= R;
  Top:= T;
  Bottom:= B;
end;

procedure TAreaSizeCalculator.CalculateAreaSize(Area: Integer;
  SizeType: TAreaSizeType);
var p: TPoint;
begin
  p:= FGM.GetAreaCenter(Area);
  CalculateAreaSize(p.X, p.Y, SizeType);
  Dec(Left); Dec(Top); Inc(Right); Inc(Bottom);
end;

function TAreaSizeCalculator.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

function TAreaSizeCalculator.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TAreaSizeCalculator.Initialize(m: TGlobalMapBase);
begin
  FGM:= m;
  Left:= m.ScaleWidth + 1;
  Top:= m.ScaleHeight + 1;
  Right:= -1;
  Bottom:= -1;
end;

{ TBacklightPngWriter }

procedure TBacklightPngWriter.DebugCheck(cX, cY: Integer);
begin
  Dec(cX, X);
  Dec(cY, Y);
  if (cX < 0) or (cX >= Backlight.Width)
      or (cY < 0) or (cY >= Backlight.Height) then
    raise Exception.Create('Error Message');
end;

function TBacklightPngWriter.DoBorder(m: TGlobalMapBase; cX, cY: Integer;
  Direct: TDirection): Boolean;
begin
  Backlight.Pixels[cX - X, cY - Y]:= CurrentColor;
  Backlight.AlphaScanline[cY - Y][cX - X]:= $ff;
  Result:= True;
end;

function TBacklightPngWriter.DoFastFill(m: TGlobalMapBase; cX, cY: Integer;
  Direct: TDirection): Boolean;
var i, j: Integer;
begin
  MoveByDirection(cX, cY, RotateRight(Direct));
  Move(FRGB, pRGBLine(Backlight.Scanline[cY - Y])^[cX - X], SizeOf(TRGBTriple));
  Backlight.AlphaScanline[cY - Y][cX - X]:= $ff;
  case Direct of
    dRight: begin
      cX:= cX - X;
      for i := cY - Y + 1 to Backlight.Height - 1 do
        if Backlight.AlphaScanline[i][cX] = $ff then begin
          for j := cY - Y + 1 to i - 1 do begin
            Move(FRGB, pRGBLine(Backlight.Scanline[j])^[cX], SizeOf(TRGBTriple));
            Backlight.AlphaScanline[j][cX]:= FFillAlpha;
          end;
          Break;
        end;
    end;
    dBottom: begin
      cY:= cY - Y;
      for i := cX - X - 1 downto 0 do
        if Backlight.AlphaScanline[cY][i] = $ff then begin
          for j := cX - X - 1 downto i + 1 do begin
            Move(FRGB, pRGBLine(Backlight.Scanline[cY])^[j], SizeOf(TRGBTriple));
            Backlight.AlphaScanline[cY][j]:= FFillAlpha;
          end;
          Break;
        end;
    end;
    dLeft: begin
      cX:= cX - X;
      for i := cY - Y - 1 downto 0 do
        if Backlight.AlphaScanline[i][cX] = $ff then begin
          for j := cY - Y - 1 downto i + 1 do begin
            Move(FRGB, pRGBLine(Backlight.Scanline[j])^[cX], SizeOf(TRGBTriple));
            Backlight.AlphaScanline[j][cX]:= FFillAlpha;
          end;
          Break;
        end;
    end;
    dUp: begin
      cY:= cY - Y;
      for i := cX - X + 1 to Backlight.Width - 1 do
        if Backlight.AlphaScanline[cY][i] = $ff then begin
          for j := cX - X + 1 to i - 1 do begin
            Move(FRGB, pRGBLine(Backlight.Scanline[cY])^[j], SizeOf(TRGBTriple));
            Backlight.AlphaScanline[cY][j]:= FFillAlpha;
          end;
          Break;
        end;
    end;
  end;

  Result:= True;
end;

procedure TBacklightPngWriter.DoFill;
var
  cY: Integer;
  cX: Integer;
  beginPos: Integer;
  i: Integer;
  oldAlpha, newAlpha: Byte;
  pColor, oldColor: PRGBTriple;
begin
  oldAlpha:= 0; //for warning
  oldColor:= nil; //for warning
  for cY := 0 to Backlight.Height - 1 do begin
    beginPos:= -1;
    for cX := 0 to Backlight.Width - 1 do begin
      newAlpha:= Backlight.AlphaScanline[cY][cX];
      if newAlpha <> 0 then begin
        pColor:= @pRGBLine(Backlight.Scanline[cY])^[cX];
        if (beginPos = -1) or (oldAlpha <> newAlpha) or
            not CompareMem(oldColor, pColor, SizeOf(TRGBTriple)) then begin
          beginPos:= cX + 1;
          oldAlpha:= newAlpha;
          oldColor:= pColor;
        end else if (cX > beginPos) and (newAlpha = $ff) then begin
          pColor:= @pRGBLine(Backlight.Scanline[cY])^[cX];
          for i := beginPos to cX - 1 do begin
            Move(pColor^, pRGBLine(Backlight.Scanline[cY])^[i], SizeOf(TRGBTriple));
            Backlight.AlphaScanline[cY][i]:= FFillAlpha;
          end;
          beginPos:= -1;
        end else
          beginPos:= cX + 1;
      end;
    end;
  end;
end;

function TBacklightPngWriter.DoPrepareToFill(m: TGlobalMapBase; cX, cY: Integer;
  Direct: TDirection): Boolean;
begin
  DebugCheck(cX, cY);
  if Backlight.AlphaScanline[cY - Y][cX - X] = 0 then begin
    Backlight.Pixels[cX - X, cY - Y]:= CurrentColor;
    Backlight.AlphaScanline[cY - Y][cX - X]:= FBorderAlpha;
  end;
  MoveByDirection(cX, cY, RotateRight(Direct));
  DebugCheck(cX, cY);
  Backlight.Pixels[cX - X, cY - Y]:= CurrentColor;
  Backlight.AlphaScanline[cY - Y][cX - X]:= $ff;
  Result:= True;
end;

procedure TBacklightPngWriter.SetBorderAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value)
  else if Value = 0 then
    Inc(Value);
  if Value = FFillAlpha then
    if Value = 1 then
      Inc(Value)
    else
      Dec(Value);
  FBorderAlpha := Value;
end;

procedure TBacklightPngWriter.SetCurrentColor(const Value: TColor);
begin
  if FCurrentColor <> Value then begin
    FCurrentColor := Value;
    with FRGB do
    begin
      rgbtBlue := GetBValue(Value);
      rgbtGreen := GetGValue(Value);
      rgbtRed := GetRValue(Value);
    end
  end;
end;

procedure TBacklightPngWriter.SetFillAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value);
  if Value = FBorderAlpha then
    if Value = 0 then
      Inc(Value)
    else
      Dec(Value);
  FFillAlpha := Value;
end;

{ TBacklightPngBase }

constructor TBacklightPngBase.Create;
begin

end;

destructor TBacklightPngBase.Destroy;
begin
  FBacklight.Free;
  inherited;
end;

procedure TBacklightPngBase.Draw(Output: TCanvas; AOffset: TPoint);
begin
  Output.Draw(X + AOffset.X, Y + AOffset.Y, Backlight);
end;

function TBacklightPngBase.GetHeight: Integer;
begin
  Result:= FBacklight.Height;
end;

function TBacklightPngBase.GetWidth: Integer;
begin
  Result:= FBacklight.Width;
end;

{ TGlobalMapBase }

function TGlobalMapBase.AddCustomBacklight(bl: TBacklightBase): Integer;
begin
  if FAutoBacklight = -1 then
    Result:= FBacklightList.Add(bl)
  else begin
    Result:= FBacklightList.Count - 1;
    FBacklightList.Insert(Result, bl);
    Inc(FAutoBacklight);
  end;
end;

constructor TGlobalMapBase.Create;
begin
  inherited;
  FAutoBacklight:= -1;
  FBacklightList.Create(2);
  FUniqueObjects:= TDictionary<string, TObject>.Create;
end;

procedure TGlobalMapBase.DeleteCustomBacklight(Index: Integer);
begin
  ExtractCustomBacklight(Index).Free;
end;

destructor TGlobalMapBase.Destroy;
var
  j: Integer;
begin
  if FBitMap <> nil then
    FreeMem(FBitMap);
  if FProcessedData <> nil then
    FreeMem(FProcessedData);

  for j := 0 to FBacklightList.Count - 1 do
    FBacklightList[j].Free;

  FUniqueObjects.OnValueNotify:= RemoveObjects;
  FUniqueObjects.Free;
  inherited;
end;

function TGlobalMapBase.ExtractCustomBacklight(Index: Integer): TBacklightBase;
begin
  Result:= nil;
  if (Index >=0) and (Index < CustomBacklightCount) then begin
    Result:= FBacklightList[Index];
    FBacklightList.Delete(Index);
    if FAutoBacklight >= 0 then
      Dec(FAutoBacklight);
  end;
end;

function TGlobalMapBase.GetAreaAt(Pos: TPoint): Integer;
var P: Pointer;
begin
  if (FBitMap = nil) or (Pos.X < 0) or (Pos.X >= Width) or (Pos.Y < 0) or (Pos.Y >= Height) then
    Exit(0);
  P:= @FBitMap[Pos.Y * Width * FBytesPerPixel + Pos.X * FBytesPerPixel];
  case FBytesPerPixel of
  1: Result:= PByte(P)^;
  2: Result:= PWord(P)^;
  4: Result:= PLongInt(P)^;
  else
    Result:= 0;
  end;
end;

function TGlobalMapBase.GetAreaCenter(Area: Integer): TPoint;
begin
  Result.Create(0, 0);
  if (FProcessedData = nil) or (Area < 1) or (Area > FAreasCount) then
    Exit;
  case FProcessedBytesPerArea of
    2: begin
      Result.X:= PByte(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^;
      Result.Y:= PByte(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 1])^;
    end;
    4: begin
      Result.X:= PWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^;
      Result.Y:= PWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 2])^;
    end;
    8: begin
      Result.X:= PLongWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^;
      Result.Y:= PLongWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 4])^;
    end;
  end;
end;

function TGlobalMapBase.GetCustomBacklight(Index: Integer): TBacklightBase;
begin
  if (Index >= 0) and (Index < CustomBacklightCount) then
    Result:= FBacklightList[Index]
  else
    Result:= nil;
end;

function TGlobalMapBase.GetCustomBacklightCount: Integer;
begin
  Result:= FBacklightList.Count;
  if FAutoBacklight >= 0 then
    Dec(Result);
end;

procedure TGlobalMapBase.Load(const AMap, ABitMap: string; AOutWidth,
  AOutHeight: Integer);
var p: TPicture;
    src: TStream;
begin
  p:= TPicture.Create;
  try
    p.LoadFromFile(AMap);
    FUniqueObjects.OnValueNotify:= RemoveObjects;
    FUniqueObjects.Clear;
    FUniqueObjects.OnValueNotify:= nil;
    src:= TFileStream.Create(ABitMap, fmOpenRead or fmShareDenyWrite);
    try
      Load(p.Graphic, src, AOutWidth, AOutHeight);
    finally
      src.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TGlobalMapBase.LoadAreaInfo(ABitMap: TStream);
var bh: TBMHeader;
    calc: TBMExtended;
    ofs, len, i: Integer;
    b, bc: Byte;
    w, wc: Word;
    d, dc: LongWord;
begin
  ABitMap.ReadBuffer(bh, SizeOf(bh));
  if bh.Sign <> 'GBM' then
    raise Exception.Create('Не верный формат карты!');
  if bh.Version > CurrentVersion then
    raise Exception.Create('Не поддерживаемая версия карты!');
  if (bh.Height <> Height) or (bh.Width <> Width) then
    raise Exception.Create('Размеры изображения и битовой карты не совпадают!');
  if FBitMap <> nil then
    FreeMem(FBitMap);
  FBytesPerPixel:= bh.bpp;
  if bh.Version > 1 then begin
    ABitMap.ReadBuffer(calc, SizeOf(calc));
    FProcessedBytesPerArea:= calc.ProcessedBPP;
    FAreasCount:= calc.AreasCount;
    if FProcessedData <> nil then begin
      FreeMem(FProcessedData);
      FProcessedData:= nil;
    end;
    GetMem(FProcessedData, FProcessedBytesPerArea * FAreasCount);
    ABitMap.ReadBuffer(FProcessedData^, FProcessedBytesPerArea * FAreasCount);
  end;

  GetMem(FBitMap, bh.Height * bh.Width * bh.bpp);
  if (bh.Version > 1) and calc.IsRLE then begin
    len:= bh.Height * bh.Width;
    ofs:= 0;
    case bh.bpp of
      1: while ofs < len do begin
        ABitMap.ReadBuffer(b, 1);
        if b > $7F then begin
          ABitMap.ReadBuffer(bc, 1);
          for i := 0 to b and $7F do
            PByte(@FBitMap[(ofs + i) * FBytesPerPixel])^:= bc;
        end else
          ABitMap.ReadBuffer(FBitMap[ofs * FBytesPerPixel], b + 1);
        Inc(ofs, b and $7F + 1);
      end;
      2: while ofs < len do begin
        ABitMap.ReadBuffer(w, 2);
        if w > $7FFF then begin
          ABitMap.ReadBuffer(wc, 2);
          for i := 0 to w and $7FFF do
            PWord(@FBitMap[(ofs + i) * FBytesPerPixel])^:= wc;
        end else
          ABitMap.ReadBuffer(FBitMap[ofs * FBytesPerPixel], (w + 1) * 2);
        Inc(ofs, w and $7FFF + 1);
      end;
      4: while ofs < len do begin
        ABitMap.ReadBuffer(d, 4);
        if d > $7FFFFFFF then begin
          ABitMap.ReadBuffer(dc, 4);
          for i := 0 to d and $7FFFFFFF do
            PLongWord(@FBitMap[(ofs + i) * FBytesPerPixel])^:= dc;
        end else
          ABitMap.ReadBuffer(FBitMap[ofs * FBytesPerPixel], (d + 1) * 4);
        Inc(ofs, d and $7FFFFFFF + 1);
      end;
    end;
    if ABitMap.Position <> ABitMap.Size then
      raise Exception.Create('Error Message');
  end else
    ABitMap.ReadBuffer(FBitMap^, bh.Height * bh.Width * bh.bpp);

  if bh.Version = 1 then
    RecreateProcessedData;
end;

procedure TGlobalMapBase.PathArea(X, Y: Integer; CoordInDefaultScale,
  UseAround: Boolean; Callback: TPathAreaCallback);
var direct: TDirection;
    area: Integer;
    pos, npos: array [0..1] of Integer;
    exPos: TPoint absolute pos;
    exNPos: TPoint absolute npos;
begin
  //данная функция всегда считает в глобальных координатах, потому не учитывает
  //смещение карты

  //Для начала поднимимся в самую верхнюю позицию провинции
  if not CoordInDefaultScale then begin
    pos[0]:= ConverToDefaultScale(X);
    pos[1]:= ConverToDefaultScale(Y);
    X:= pos[0];
  end else begin
    pos[0]:= X;
    pos[1]:= Y;
  end;

  area:= GetAreaAt(exPos);
  while (pos[1] > 0) and (GetAreaAt(exPos) = area) do
    Dec(pos[1]);
  if GetAreaAt(exPos) <> area then
    Inc(pos[1]);
  Y:= pos[1];

  //тут поищем все провинции которые соседничают с текущей
  //мы всегда находимся в самой правой верхней точке провинции
  //будем обходить по часовой стрелке, ориентируясь по левой стороне
  direct:= dRight; //смотрим направо
  //0 - право
  //1 - низ
  //2 - лево
  //3 - верх
  repeat
    //проверим по левую руку
    npos:= pos;
    MoveByDirection(npos[0], npos[1], RotateLeft(direct));
    //npos[(direct + 4 - 1) mod 2]:= npos[(direct + 4 - 1) mod 2] + offsets[(direct + 4 - 1) mod 4];
    if (npos[0] >= 0) and (npos[0] < Width) and (npos[1] >= 0) and (npos[1] < Height) then begin
      if GetAreaAt(exNPos) <> area then begin
        if not Callback(Self, ConvertToCurrentScale(npos[0]), ConvertToCurrentScale(npos[1]), direct) then
          Break;
      end else begin
        direct:= RotateLeft(direct);//поворачиваем на лево
        pos:= npos;
        continue;
      end;
    end else if UseAround then
      if not Callback(Self, ConvertToCurrentScale(npos[0]), ConvertToCurrentScale(npos[1]), direct) then
        Break;
    //а теперь по прямой
    npos:= pos;
    MoveByDirection(npos[0], npos[1], direct);
    if (npos[0] >= 0) and (npos[0] < Width) and (npos[1] >= 0) and (npos[1] < Height) then begin
      if GetAreaAt(exNPos) <> area then begin
        //вызов не нужен, т.к. после поворота на право, будет проверка левой стороны и тогда произойдет вызов
        {if not Callback(Self, npos[0], npos[1], direct) then
          Break;}
        direct:= RotateRight(direct);//поворачиваем на право
      end else
        pos:= npos;
    end else
      direct:= RotateRight(direct);//поворачиваем на право
  until (pos[0] = x) and (pos[1] = y) and (direct = dRight);//вернулись в начало, значит обошли все
end;

procedure TGlobalMapBase.PathArea(Area: Integer; UseAround: Boolean;
  Callback: TPathAreaCallback);
var exPos: TPoint;
begin
  exPos:= GetAreaCenter(Area);
  PathArea(exPos.X, exPos.Y, True, UseAround, Callback);
end;

procedure TGlobalMapBase.RecreateProcessedData;
var maxIndex: Integer;
  i, m, n: Integer;
  j, C: Integer;
  size: TAreaSizeCalculator;
  buf: array of Word;
  buf2: array of array of Word;
  l: Integer;
  k: Integer;
  found: Boolean;
begin
  maxIndex:= 0;
  case FBytesPerPixel of
  1:
    for i := 0 to Height - 1 do
      for j := 0 to Width - 1 do
        if PByte(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^ > maxIndex then
          maxIndex:= PByte(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^;
  2:
    for i := 0 to Height - 1 do
      for j := 0 to Width - 1 do
        if PWord(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^ > maxIndex then
          maxIndex:= PWord(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^;
  4:
    for i := 0 to Height - 1 do
      for j := 0 to Width - 1 do
        if PLongInt(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^ > maxIndex then
          maxIndex:= PLongInt(@FBitMap[i * Width * FBytesPerPixel + j * FBytesPerPixel])^;
  end;

  i:= Width;
  C := 0;
  repeat
    i := i div 256;
    Inc(C);
  until i = 0;
  if C = 3 then
    C := 4;

  FProcessedBytesPerArea:= C * 2;

  i:= Height;
  C := 0;
  repeat
    i := i div 256;
    Inc(C);
  until i = 0;
  if C = 3 then
    C := 4;

  if C * 2 > FProcessedBytesPerArea then
    FProcessedBytesPerArea:= C * 2;

  if FProcessedData <> nil then begin
    FreeMem(FProcessedData);
    FProcessedData:= nil;
  end;

  FAreasCount:= maxIndex;
  GetMem(FProcessedData, FProcessedBytesPerArea * maxIndex);
  FillChar(FProcessedData^, FProcessedBytesPerArea * maxIndex, 0);

  for m := 0 to Height - 1 do
    for n := 0 to Width - 1 do begin
      C:= GetAreaAt(Point(n, m));
      if (C <> 0) and (GetAreaCenter(C) = Point(0, 0)) then begin
        size.Initialize(Self);
        size.CalculateAreaSize(n, m, astOuterWithUnbound);
        Inc(size.Right);
        Inc(size.Bottom);
        buf:= nil;
        SetLength(buf, size.Width * size.Height);
        PathArea(n, m, True, True, function (m: TGlobalMapBase; cX, cY: Integer; Direct: TDirection): Boolean
          begin
            buf[(cY - size.Top) * size.Width + cX - size.Left]:= 1;
            Result:= True;
          end);
        SetLength(buf2, size.Height, size.Width);
        for i := 0 to size.Height - 1 do
          Move(buf[i * size.Width], buf2[i][0], SizeOf(Word) * size.Width);
        //расчитываем размер по горизонтальным линиям
        for i := 0 to size.Height - 1 do begin
          l:= 0;
          for j := 0 to size.Width - 1 do begin
            if buf[i * size.Width + j] = 1 then
              if l <=1 then
                l:= 1
              else begin
                for k := 1 to l div 2 do
                  buf[i * size.Width + j - k]:= k + 1;
                for k := l div 2 downto 1 do
                  buf[i * size.Width + j - l + k]:= k + 1;
                l:= 0;
              end
            else if l > 0 then begin
              Inc(l);
            end;
          end;
        end;
        found:= False;
        for i := 0 to size.Height - 1 do
          Move(buf[i * size.Width], buf2[i][0], SizeOf(Word) * size.Width);
        //ищем достаточно удалённую от границы точку по вертикали и по горизонтали
        //нужен ли этот код, может стоит брать любую точку? илии вообще какую-то определённую,
        //которая поможет высчитать центр масс например или самую близкую к нему?
        for j := 0 to size.Width - 1 do begin
          l:= 0;
          for i := 0 to size.Height - 1 do begin
            if buf[i * size.Width + j] <= 1 then
              if l = 0 then
                l:= buf[i * size.Width + j]
              else
                l:= 0
            else if l > 0 then begin
              Inc(l);
              if (l >= 3) and (buf[i * size.Width + j] >= 3) then begin
                SetAreaCenter(C, j + size.Left, i + size.Top);
                found:= True;
                Break;
              end;
            end;
          end;
          if found then
            Break;
        end;
        if not found then
          for j := 0 to size.Width - 1 do
            for i := 0 to size.Height - 1 do
              if buf[i * size.Width + j] > 1 then begin
                SetAreaCenter(C, j + size.Left, i + size.Top);
                Break;
              end;
      end;
    end;

  for i := 1 to maxIndex do
    if GetAreaCenter(i) = Point(0, 0) then
      raise Exception.Create('Необработаны все зоны');
end;

procedure TGlobalMapBase.RefreshScales;
begin

end;

procedure TGlobalMapBase.RemoveObjects(Sender: TObject; const Item: TObject;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: ;
    cnRemoved: Item.Free;
    cnExtracted: ;
  end;
end;

procedure TGlobalMapBase.SetAreaCenter(Area, X, Y: Integer);
begin
  case FProcessedBytesPerArea of
    2: begin
      PByte(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^:= X;
      PByte(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 1])^:= Y;
    end;
    4: begin
      PWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^:= X;
      PWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 2])^:= Y;
    end;
    8: begin
      PLongWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea])^:= X;
      PLongWord(@FProcessedData[(Area - 1) * FProcessedBytesPerArea + 4])^:= Y;
    end;
  end;
end;

procedure TGlobalMapBase.SetCustomBacklight(Index: Integer;
  const Value: TBacklightBase);
var old: TBacklightBase;
begin
  old:= GetCustomBacklight(Index);
  if (Index >= 0) and (Index < CustomBacklightCount) then
    FBacklightList[Index]:= Value;
  old.Free;
end;

{ TBacklightArrayWriter }

function TBacklightArrayWriter.DoBorder(m: TGlobalMapBase; cX, cY: Integer;
  Direct: TDirection): Boolean;
begin
  Backlight[cX - X + (cY - Y) * Width]:= TColor(CurrentColor or TColor($ff000000));
  Result:= True;
end;

procedure TBacklightArrayWriter.DoFill;
var
  cY: Integer;
  cX: Integer;
  beginPos: Integer;
  i: Integer;
  pColor, oldColor, nColor: TColor;
begin
  oldColor:= 0; //for warning
  for cY := 0 to Height - 1 do begin
    beginPos:= -1;
    for cX := 0 to Width - 1 do begin
      pColor:= Backlight[cY * Width + cX];
      if pColor and $FF000000 <> 0 then begin
        if (beginPos = -1) or (oldColor <> pColor) then begin
          beginPos:= cX + 1;
          oldColor:= pColor;
        end else if (cX > beginPos) and (pColor and $ff000000 = $ff000000) then begin
          nColor:= pColor and $FFFFFF + FFillAlpha shl 24;
          for i := beginPos to cX - 1 do
            Backlight[cY * Width + i]:= nColor;
          if oldColor <> pColor then
            beginPos:= -1
          else
            beginPos:= cX + 1;
        end else
          beginPos:= cX + 1;
      end;
    end;
  end;
end;

function TBacklightArrayWriter.DoPrepareToFill(m: TGlobalMapBase; cX,
  cY: Integer; Direct: TDirection): Boolean;
var ofs: Integer;
begin
  ofs:= (cY - Y) * Width + cX - X;
  //DebugCheck(cX, cY);
  if Backlight[ofs] and $ff000000 = 0 then
    Backlight[ofs]:= CurrentColor or FBorderAlpha shl 24;
  MoveByDirection(cX, cY, RotateRight(Direct));
  //DebugCheck(cX, cY);
  ofs:= (cY - Y) * Width + cX - X;
  Backlight[ofs]:= TColor(CurrentColor or TColor($ff000000));
  Result:= True;
end;

procedure TBacklightArrayWriter.SetBorderAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value)
  else if Value = 0 then
    Inc(Value);
  if Value = FFillAlpha then
    if Value = 1 then
      Inc(Value)
    else
      Dec(Value);
  FBorderAlpha := Value;
end;

procedure TBacklightArrayWriter.SetFillAlpha(Value: Byte);
begin
  if Value = $ff then
    Dec(Value);
  if Value = FBorderAlpha then
    if Value = 0 then
      Inc(Value)
    else
      Dec(Value);
  FFillAlpha := Value;
end;

end.
