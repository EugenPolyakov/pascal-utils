unit GlobalMap;

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Collections, System.Types,
     Vcl.Graphics, Vcl.Imaging.pngimage, RecordUtils, GlobalMapFile;

type
  TGlobalMapBase = class;

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
    procedure CalculateAreaSize(var ACalc: TAreaSizeCalculator; Map: TGlobalMapBase; Area: Integer; SizeType: TAreaSizeType);
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
    function DoBorder(cX, cY: Integer; Direct: TDirection): Boolean;
    function DoPrepareToFill(cX, cY: Integer; Direct: TDirection): Boolean;
    function DoFastFill(cX, cY: Integer; Direct: TDirection): Boolean;
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

  TScrollGraphic = class
  private
    FCurrX, FCurrY, FDragX, FDragY: Integer;
    FUseDrag, FIsDrag: Boolean;
    FOutWidth, FOutHeight: Integer;
    procedure SetOutHeight(const Value: Integer);
    procedure SetOutWidth(const Value: Integer);
  protected
    FWidth, FHeight: Integer;
    FScale: Integer;
    FScaleCoefficient: Extended;
    procedure SetScale(Value: Integer); virtual;
    procedure CorrectCurrX;
    procedure CorrectCurrY;
  public
    property CurrX: Integer read FCurrX;
    property CurrY: Integer read FCurrY;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    function ConvertToDefaultScale(Value: Integer): Integer;
    function ConvertToCurrentScale(Value: Integer): Integer;
    property Scale: Integer read FScale write SetScale;
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
    procedure DragTo(X, Y: Integer);
    constructor Create;
  end;

  TGlobalMapBase = class (TScrollGraphic)
  private
    function GetAreasCount: Integer; inline;
  protected
    FAutoBacklight: Integer;
    FBacklightList: TListRecord<TBacklightBase>;
    FUniqueObjects: TDictionary<string, TObject>;
    FBitMap: TBitMapFileData;  //Почти свой формат))
    FBitmapOwner: Boolean;
    FOffsetX: Integer;
    FOffsetY: Integer;
    function GetScaleHeight: Integer; virtual; abstract;
    function GetScaleWidth: Integer; virtual; abstract;
    function GetCustomBacklight(Index: Integer): TBacklightBase;
    function GetCustomBacklightCount: Integer; inline;
    procedure SetCustomBacklight(Index: Integer; const Value: TBacklightBase);
    procedure RemoveObjects(Sender: TObject; const Item: TObject; Action: TCollectionNotification); virtual;
    procedure Load(ABitMap: TStream; AOutWidth, AOutHeight: Integer);
    procedure ClearUniqueObjects;
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
    destructor Destroy; override;
    function GetAreaAt(const Pos: TPoint): Integer;//Получить ID области в координатах карты
    procedure SetBacklight(Pos: TPoint; Color: TColor); virtual; abstract;//подсветить область под указанными координатам
    property AreasCount: Integer read GetAreasCount;
    function GetAreaCenter(Area: Integer): TPoint;
    procedure RefreshScales; virtual;
    procedure PathArea(X, Y: Integer; UseAround: Boolean; Callback: TPathAreaCallback); overload;
    procedure PathArea(Area: Integer; UseAround: Boolean; Callback: TPathAreaCallback); overload;
  end;

  TGlobalMap = class (TGlobalMapBase)
  private
    //Все имеющиеся косяки реализации сейчас нет смысла править,
    //в будущем при их исправлении интерфейс меняться не будет,
    //а для их исправления нужны сложные структуры данных, которые на данном этапе слишком накладно делать
    FMap: array of TBitmap; //Для очень больших карт простой bmp тоже не катит нужно сделать кеширование
    FBacklightGenerator: TBacklightAutoGenerator;
    FOutput: TCanvas;
    procedure SetMap(const Value: TBitmap);
    function GetMap: TBitmap;
  protected
    function GetScaleHeight: Integer; override;
    function GetScaleWidth: Integer; override;
    procedure SetScale(Value: Integer); override;
    property Map: TBitmap read GetMap write SetMap;
    procedure ShowBacklight(Output: TCanvas; AOffset: TPoint);
  public
    property BacklightGenerator: TBacklightAutoGenerator read FBacklightGenerator write FBacklightGenerator;
    constructor Create(AOutput: TCanvas);
    procedure Load(const AMap, ABitMap: string; AOutWidth, AOutHeight: Integer);
    destructor Destroy; override;
    procedure StopBacklight;
    procedure UpdateImage; overload; override;//Обновить изображение карты
    procedure SetBacklight(Pos: TPoint; Color: TColor); override;//подсветить область под указанными координатам
    //Добавить объект поверх карты
    //ID - переводится в верхний регистр
    //если добавть обект, который уже есть, то старый удалится и добавится новый
    procedure SetUniqueObject(X, Y: Integer; const ID: string; AObject: TGraphic);
    //Удалить объект
    procedure DeleteUniqueObject(const ID: string);
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
  Offsets: array [TDirection] of Integer = (1, 1, -1, -1);

implementation

uses Math, SysTypes;

{ TScrollGraphic }

procedure TScrollGraphic.AutoDrag(X, Y: Integer; Update: Boolean);
begin
  if not FUseDrag then Exit;

  DragTo(ConvertToDefaultScale(FDragX - X), ConvertToDefaultScale(FDragY - Y));

  FIsDrag:= (FDragX <> X) or (FDragY <> Y);
  if FIsDrag and Update then
    UpdateImage;
  FDragX:= X;
  FDragY:= Y;
end;

function TScrollGraphic.ConvertToReal(X, Y: Integer): TPoint;
begin
  Result.X:= ConvertToDefaultScale(X) + FCurrX;
  Result.Y:= ConvertToDefaultScale(Y) + FCurrY;
end;

procedure TScrollGraphic.CorrectCurrX;
begin
  if (FCurrX < 0) or (Width <= ConvertToDefaultScale(FOutWidth)) then
    FCurrX:= 0
  else if FCurrX > Width - ConvertToDefaultScale(FOutWidth) then
    FCurrX:= Width - ConvertToDefaultScale(FOutWidth);
end;

procedure TScrollGraphic.CorrectCurrY;
begin
  if (FCurrY < 0) or (Height <= ConvertToDefaultScale(FOutHeight)) then
    FCurrY:= 0
  else if FCurrY > Height - ConvertToDefaultScale(FOutHeight) then
    FCurrY:= Height - ConvertToDefaultScale(FOutHeight);
end;

constructor TScrollGraphic.Create;
begin
  FScaleCoefficient:= 1;
end;

procedure TScrollGraphic.DragTo(X, Y: Integer);
begin
  Inc(FCurrX, X);
  CorrectCurrX;
  Inc(FCurrY, Y);
  CorrectCurrY;
end;

function TScrollGraphic.ConvertToCurrentScale(Value: Integer): Integer;
begin
  Result:= Round(Value * FScaleCoefficient);
end;

function TScrollGraphic.ConvertToDefaultScale(Value: Integer): Integer;
begin
  Result:= Round(Value / FScaleCoefficient);
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

procedure TScrollGraphic.SetScale(Value: Integer);
var oldX, oldY: Integer;
    v: Extended;
    i: Integer;
begin
  oldX:= ConvertToDefaultScale(FOutWidth div 2);
  oldY:= ConvertToDefaultScale(FOutHeight div 2);

  FScale:= Value;

  v:= 1;
  for i := 1 to Scale do
    v:= v * 7 / 8;
  FScaleCoefficient:= v;

  StopDrag;
  if Width > 0 then begin
    FCurrX:= FCurrX + oldX - ConvertToDefaultScale(FOutWidth div 2);
    FCurrY:= FCurrY + oldY - ConvertToDefaultScale(FOutHeight div 2);
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
  SetLength(FMap, 1);
  FMap[0]:= TBitmap.Create;
end;

procedure TGlobalMap.DeleteUniqueObject(const ID: string);
var s: string;
    b: TUniqueObject;
begin
  s:= UpperCase(ID);
  if FUniqueObjects.TryGetValue(s, TObject(b)) then begin
    //восстанавливаем старую часть карты
    FMap[0].Canvas.CopyRect(Rect(b.X, b.Y, b.X + b.Width, b.Y + b.Height),
        b.Canvas, Rect(0, 0, b.Width, b.Height));
    FUniqueObjects.Remove(s);
    b.Free;
  end;
end;

destructor TGlobalMap.Destroy;
var
  i: Integer;
begin
  for i := Low(FMap) to High(FMap) do
    FMap[i].Free;

  inherited;
end;

function TGlobalMap.GetMap: TBitmap;
begin
  Result:= FMap[0];
end;

function TGlobalMap.GetScaleHeight: Integer;
begin
  Result:= FMap[Scale].Height;
end;

function TGlobalMap.GetScaleWidth: Integer;
begin
  Result:= FMap[Scale].Width;
end;

procedure TGlobalMap.Load(const AMap, ABitMap: string; AOutWidth, AOutHeight: Integer);
var p: TPicture;
    src: TStream;
begin
  p:= TPicture.Create;
  try
    p.LoadFromFile(AMap);
    src:= TFileStream.Create(ABitMap, fmOpenRead or fmShareDenyWrite);
    try
      inherited Load(src, AOutWidth, AOutHeight);

      if (p.Height <> Height) or (p.Width <> Width) then
        raise Exception.Create('Размеры изображения и битовой карты не совпадают!');

      FMap[0].Height:= p.Height;
      FMap[0].Width:= p.Width;
      FMap[0].Canvas.Draw(0, 0, p.Graphic);

      RefreshScales;
    finally
      src.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TGlobalMap.RefreshScales;
var
  i: Integer;
begin
  for i := 1 to High(FMap) do
    FreeAndNil(FMap[i]);

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
  FMap[0]:= Value;
end;

procedure TGlobalMap.SetScale(Value: Integer);
begin
  inherited;
  if Length(FMap) <= Value then
    SetLength(FMap, Value + 1);
  if FMap[Value] = nil then begin
    FMap[Value]:= TBitmap.Create;
    FMap[Value].SetSize(ConvertToCurrentScale(Width), ConvertToCurrentScale(Height));
    SetStretchBltMode(FMap[Value].Canvas.Handle, {STRETCH_}HALFTONE);

    FMap[Value].Canvas.CopyRect(Rect(0,0, FMap[Value].Width, FMap[Value].Height), FMap[0].Canvas,
      Rect(0, 0, FMap[0].Width, FMap[0].Height));
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
    FMap[0].Canvas.CopyRect(Rect(b.X, b.Y, b.X + b.Width, b.Y + b.Height),
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
      FMap[0].Canvas, Rect(X, Y, X + AObject.Width, Y + AObject.Height));
  //рисуем объект на карте
  FMap[0].Canvas.Draw(X, Y, AObject);
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

  r.Initialize(Map.ScaleWidth, Map.ScaleHeight);
  CalculateAreaSize(r, Map, Area, astOuter);
  FX:= r.Left;
  FY:= r.Top;

  FBacklight:= TPngImage.CreateBlank(COLOR_RGBALPHA, 8, r.Width + 1, r.Height + 1);

  //зарисовываем
  Map.PathArea(Area, False, function (cX, cY: Integer; Direct: TDirection): Boolean
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
  r.Initialize(Map.ScaleWidth, Map.ScaleHeight);
  CalculateAreaSize(r, Map, Area, astOuterWithUnbound);

  exAreas:= GetExtendedAreaList(Area);

  for i := 0 to High(exAreas) do
    CalculateAreaSize(r, Map, exAreas[i].Area, astOuterWithUnbound);

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

{ TBacklightPngWriter }

procedure TBacklightPngWriter.DebugCheck(cX, cY: Integer);
begin
  Dec(cX, X);
  Dec(cY, Y);
  if (cX < 0) or (cX >= Backlight.Width)
      or (cY < 0) or (cY >= Backlight.Height) then
    raise Exception.Create('Error Message');
end;

function TBacklightPngWriter.DoBorder(cX, cY: Integer;
  Direct: TDirection): Boolean;
begin
  Backlight.Pixels[cX - X, cY - Y]:= CurrentColor;
  Backlight.AlphaScanline[cY - Y][cX - X]:= $ff;
  Result:= True;
end;

function TBacklightPngWriter.DoFastFill(cX, cY: Integer;
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

function TBacklightPngWriter.DoPrepareToFill(cX, cY: Integer;
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

procedure TGlobalMapBase.ClearUniqueObjects;
begin
  FUniqueObjects.OnValueNotify:= RemoveObjects;
  FUniqueObjects.Clear;
  FUniqueObjects.OnValueNotify:= nil;
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
  if FBitmapOwner then
    FreeAndNil(FBitMap);

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

function TGlobalMapBase.GetAreaAt(const Pos: TPoint): Integer;
begin
  if FBitMap = nil then
    Exit(0);

  Result:= FBitMap.GetAreaAt(Pos);
end;

function TGlobalMapBase.GetAreaCenter(Area: Integer): TPoint;
begin
  if FBitMap = nil then
    Exit(TPoint.Create(0, 0));
  Result:= FBitMap.GetAreaCenter(Area);
end;

function TGlobalMapBase.GetAreasCount: Integer;
begin
  if FBitMap = nil then
    Result:= 0
  else
    Result:= FBitMap.AreasCount;
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

procedure TGlobalMapBase.Load(ABitMap: TStream; AOutWidth, AOutHeight: Integer);
begin
  ClearUniqueObjects;

  if FBitmapOwner then
    FreeAndNil(FBitMap);

  FBitmapOwner:= True;
  FBitMap:= TBitMapFileData.Create;
  FBitMap.LoadFromStream(ABitmap);
  FHeight:= FBitMap.Height;
  FWidth:= FBitMap.Width;

  OutWidth:= AOutWidth;
  OutHeight:= AOutHeight;
end;

procedure TGlobalMapBase.PathArea(X, Y: Integer; UseAround: Boolean; Callback: TPathAreaCallback);
var direct: TDirection;
    area: Integer;
    pos, npos: array [0..1] of Integer;
    exPos: TPoint absolute pos;
    exNPos: TPoint absolute npos;
begin
  //данная функция всегда считает в глобальных координатах, потому не учитывает
  //смещение карты

  //Для начала поднимимся в самую верхнюю позицию провинции
  pos[0]:= X;
  pos[1]:= Y;
  area:= GetAreaAt(exPos);
  repeat
    exNPos:= exPos;
    while (pos[1] > 0) and (GetAreaAt(exPos) = area) do
      Dec(pos[1]);
    if GetAreaAt(exPos) <> area then
      Inc(pos[1]);

    while (pos[0] > 0) and (GetAreaAt(exPos) = area) do
      Dec(pos[0]);
    if GetAreaAt(exPos) <> area then
      Inc(pos[0]);
  until (exNPos.X = pos[0]) and (exNPos.Y = pos[1]);
  X:= pos[0];
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
        if not Callback(ConvertToCurrentScale(npos[0]), ConvertToCurrentScale(npos[1]), direct) then
          Break;
      end else begin
        direct:= RotateLeft(direct);//поворачиваем на лево
        pos:= npos;
        continue;
      end;
    end else if UseAround then
      if not Callback(ConvertToCurrentScale(npos[0]), ConvertToCurrentScale(npos[1]), direct) then
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
  PathArea(exPos.X, exPos.Y, UseAround, Callback);
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

{ TBacklightAuto }

procedure TBacklightAuto.CalculateAreaSize(var ACalc: TAreaSizeCalculator; Map: TGlobalMapBase; Area: Integer;
  SizeType: TAreaSizeType);
var p: TPoint;
begin
  p:= Map.GetAreaCenter(Area);
  ACalc.CalculateAreaSize(p.X, p.Y, SizeType, Map.PathArea);
  Dec(ACalc.Left); Dec(ACalc.Top); Inc(ACalc.Right); Inc(ACalc.Bottom);
end;

end.
