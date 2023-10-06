unit GlobalMapFile;

interface

uses
  System.SysUtils, System.Classes, System.Types;

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

  TPathAreaCallback = reference to function (CurrX, CurrY: Integer; Direct: TDirection): Boolean;

  TBitMapFileData = class
  strict private
    FHeight: Integer;
    FWidth: Integer;
    FBytesPerPixel: Byte;
    FBitMap: PAnsiChar;
    FProcessedBytesPerArea: Integer;
    FAreasCount: Integer;
    FProcessedData: PAnsiChar;
  strict protected
    function GetScanLine(Y: Integer): PAnsiChar; inline;
    procedure SetAreaCenter(Area: Integer; X, Y: Integer);
  public
    property AreasCount: Integer read FAreasCount;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    property BytesPerPixel: Byte read FBytesPerPixel;
    property ScanLine[Y: Integer]: PAnsiChar read GetScanLine;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream; AUseRLE: Boolean);
    procedure RecreateProcessedData;
    function GetAreaCenter(Area: Integer): TPoint; inline;
    function GetAreaAt(const Pos: TPoint): Integer; inline;//Получить ID области в координатах карты
    procedure PathArea(X, Y: Integer; UseAround: Boolean; Callback: TPathAreaCallback); overload;
    destructor Destroy; override;
  end;

  TPathAreaByPoint = procedure (X, Y: Integer; UseAround: Boolean; Callback: TPathAreaCallback) of object;

  TAreaSizeType = (astInner, astOuter, astOuterWithUnbound);

  TAreaSizeCalculator = record
  private
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
  public
    Left, Top, Right, Bottom: Longint;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    procedure Initialize(AWidth, AHeight: Integer);
    procedure CalculateAreaSize(X, Y: Integer; SizeType: TAreaSizeType; APathMethod: TPathAreaByPoint);
  end;

const
  CurrentVersion = 2;

procedure MoveByDirection(var X, Y: Integer; Direction: TDirection); overload; inline;
function RotateLeft(Direction: TDirection): TDirection; inline;
function RotateRight(Direction: TDirection): TDirection; inline;

implementation

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

{ TBitMapFileData }

destructor TBitMapFileData.Destroy;
begin
  if FBitMap <> nil then
    FreeMem(FBitMap);
  if FProcessedData <> nil then
    FreeMem(FProcessedData);

  inherited;
end;

function TBitMapFileData.GetAreaAt(const Pos: TPoint): Integer;
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

function TBitMapFileData.GetAreaCenter(Area: Integer): TPoint;
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

function TBitMapFileData.GetScanLine(Y: Integer): PAnsiChar;
begin
  Result:= @FBitMap[Y * FBytesPerPixel * FWidth];
end;

procedure TBitMapFileData.LoadFromStream(AStream: TStream);
var bh: TBMHeader;
    calc: TBMExtended;
    ofs, len, i: Integer;
    b, bc: Byte;
    w, wc: Word;
    d, dc: LongWord;
begin
  AStream.ReadBuffer(bh, SizeOf(bh));
  if bh.Sign <> 'GBM' then
    raise Exception.Create('Не верный формат карты!');
  if bh.Version > CurrentVersion then
    raise Exception.Create('Не поддерживаемая версия карты!');
  FHeight:= bh.Height;
  FWidth:= bh.Width;
  if FBitMap <> nil then
    FreeMem(FBitMap);
  FBytesPerPixel:= bh.bpp;
  if bh.Version > 1 then begin
    AStream.ReadBuffer(calc, SizeOf(calc));
    FProcessedBytesPerArea:= calc.ProcessedBPP;
    FAreasCount:= calc.AreasCount;
    if FProcessedData <> nil then begin
      FreeMem(FProcessedData);
      FProcessedData:= nil;
    end;
    GetMem(FProcessedData, FProcessedBytesPerArea * FAreasCount);
    AStream.ReadBuffer(FProcessedData^, FProcessedBytesPerArea * FAreasCount);
  end;

  GetMem(FBitMap, bh.Height * bh.Width * bh.bpp);
  if (bh.Version > 1) and calc.IsRLE then begin
    len:= bh.Height * bh.Width;
    ofs:= 0;
    case bh.bpp of
      1: while ofs < len do begin
        AStream.ReadBuffer(b, 1);
        if b > $7F then begin
          AStream.ReadBuffer(bc, 1);
          for i := 0 to b and $7F do
            PByte(@FBitMap[(ofs + i) * FBytesPerPixel])^:= bc;
        end else
          AStream.ReadBuffer(FBitMap[ofs * FBytesPerPixel], b + 1);
        Inc(ofs, b and $7F + 1);
      end;
      2: while ofs < len do begin
        AStream.ReadBuffer(w, 2);
        if w > $7FFF then begin
          AStream.ReadBuffer(wc, 2);
          for i := 0 to w and $7FFF do
            PWord(@FBitMap[(ofs + i) * FBytesPerPixel])^:= wc;
        end else
          AStream.ReadBuffer(FBitMap[ofs * FBytesPerPixel], (w + 1) * 2);
        Inc(ofs, w and $7FFF + 1);
      end;
      4: while ofs < len do begin
        AStream.ReadBuffer(d, 4);
        if d > $7FFFFFFF then begin
          AStream.ReadBuffer(dc, 4);
          for i := 0 to d and $7FFFFFFF do
            PLongWord(@FBitMap[(ofs + i) * FBytesPerPixel])^:= dc;
        end else
          AStream.ReadBuffer(FBitMap[ofs * FBytesPerPixel], (d + 1) * 4);
        Inc(ofs, d and $7FFFFFFF + 1);
      end;
    end;
    if AStream.Position <> AStream.Size then
      raise Exception.Create('Error Message');
  end else
    AStream.ReadBuffer(FBitMap^, bh.Height * bh.Width * bh.bpp);

  if bh.Version = 1 then
    RecreateProcessedData;
end;

procedure TBitMapFileData.PathArea(X, Y: Integer; UseAround: Boolean; Callback: TPathAreaCallback);
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
        if not Callback(npos[0], npos[1], direct) then
          Break;
      end else begin
        direct:= RotateLeft(direct);//поворачиваем на лево
        pos:= npos;
        continue;
      end;
    end else if UseAround then
      if not Callback(npos[0], npos[1], direct) then
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

procedure TBitMapFileData.RecreateProcessedData;
var maxDeep: Integer;
  buf2: array of array of Word;

  procedure FillDeep(x, y, l: Integer);
  var k, e: Integer;
  begin
    e:= l div 2;            //12321   123321  121
    if maxDeep < e then
      maxDeep:= e;
    for k := 1 to e + l mod 2 do
      buf2[y, x - k]:= k + 1;
    for k := e downto 1 do
      buf2[y, x - l - 1 + k]:= k + 1;
  end;
var maxIndex: Integer;
  i, m, n: Integer;
  j, C: Integer;
  size: TAreaSizeCalculator;
  l: Integer;
  e: Integer;
  found: Boolean;
  Deep2, Deep1, w: Integer;
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
        size.Initialize(Width, Height);
        size.CalculateAreaSize(n, m, astOuterWithUnbound, PathArea);
        Inc(size.Right);
        Inc(size.Bottom);
        //центр просто центр общего периметра (не центр масс)
        SetAreaCenter(C, size.GetWidth div 2 + size.Left, size.GetHeight div 2 + size.Top);

        if GetAreaAt(GetAreaCenter(C)) = C then
          Continue;

        //если центр не попал на саму зону, то нужно его сдвинуть
        buf2:= nil;
        SetLength(buf2, size.Height, size.Width);
        //расчитываем размер по горизонтальным линиям
        maxDeep:= 0;
        for i := 0 to size.Height - 1 do begin                                                  // 1111111111    1
          l:= 0;                                                                                //12        21  121
          for j := 0 to size.Width - 1 do begin
            if l > 0 then begin
              if GetAreaAt(Point(j + size.Left, i + size.Top)) <> C then begin
                FillDeep(j, i, l);
                l:= 0;
              end else
                Inc(l);
            end else if GetAreaAt(Point(j + size.Left, i + size.Top)) = C then
              l:= 1;
          end;
        end;
        found:= False;
        //ищем достаточно удалённую от границы точку по вертикали и по горизонтали
        //нужен ли этот код, может стоит брать любую точку? илии вообще какую-то определённую,
        //которая поможет высчитать центр масс например или самую близкую к нему?
        Deep2:= maxDeep * maxDeep div 2; //апроксимация центра масс, предполагается если глубина будет хотя бы больше самой большой глубины по X, то уже достаточно близко к центру
        Deep1:= Deep2 div 2 + 1; //упрощённая проверка уменьшающая общую глубину в 2 раза, на случай если первая не найдена
        maxDeep:= 0;
        for j := 0 to size.Width - 1 do begin
          l:= 0;
          for i := 0 to size.Height - 1 do begin
            if buf2[i, j] > 1 then
              Inc(l)
            else begin
              if l > 0 then begin
                e:= l div 2;
                w:= e * buf2[(i - e), j];
                if w > Deep2 then begin
                  SetAreaCenter(C, j + size.Left, i - e + size.Top);
                  found:= True;
                  Break;
                end else if (w > Deep1) and (w > maxDeep) then begin
                  SetAreaCenter(C, j + size.Left, i - e + size.Top);
                  maxDeep:= w;
                end;
              end;
              l:= 0;
            end;
          end;
          if found then
            Break;
        end;
        //если вообще не нашли никакого центра, то тыкаем в первую походящую точку
        if GetAreaAt(GetAreaCenter(C)) <> C then
          for j := 0 to size.Width - 1 do
            for i := 0 to size.Height - 1 do
              if buf2[i, j] > 1 then begin
                SetAreaCenter(C, j + size.Left, i + size.Top);
                Break;
              end;
      end;
    end;

  for i := 1 to maxIndex do
    if GetAreaCenter(i) = Point(0, 0) then
      raise Exception.Create('Необработаны все зоны');
end;

procedure TBitMapFileData.SaveToStream(AStream: TStream; AUseRLE: Boolean);
var
  bh: TBMHeader;
  be: TBMExtended;
  i, p, last, cur, maxLen, mask, Count: Integer;
  isRepeat: Boolean;
begin
  bh.Width := Width;
  bh.Height := Height;
  bh.bpp := FBytesPerPixel;
  bh.Sign := 'GBM';
  bh.Version := 2;
  AStream.WriteBuffer(bh, SizeOf(bh));
  be.ProcessedBPP := FProcessedBytesPerArea;
  be.AreasCount := FAreasCount;
  be.IsRLE := AUseRLE;
  AStream.WriteBuffer(be, SizeOf(be));
  AStream.WriteBuffer(FProcessedData^, FAreasCount * FProcessedBytesPerArea);
  if not AUseRLE then
    AStream.WriteBuffer(FBitMap^, Height * Width * FBytesPerPixel)
  else
  begin
    p := 0;
    last := GetAreaAt(Point(0, 0));
    maxLen := 1 shl (FBytesPerPixel * 8 - 1);
    mask := maxLen;
    isRepeat := False;
    for i := 1 to Height * Width - 1 do
    begin
      case FBytesPerPixel of
        1:
          cur := PByte(@FBitMap[i * FBytesPerPixel])^;
        2:
          cur := PWord(@FBitMap[i * FBytesPerPixel])^;
        4:
          cur := PLongInt(@FBitMap[i * FBytesPerPixel])^;
      else
        cur := 0;
      end;
      if last <> cur then
      begin
        if isRepeat then
        begin
          Count := (i - p - 1) or mask;
          AStream.WriteBuffer(Count, FBytesPerPixel);
          AStream.WriteBuffer(last, FBytesPerPixel);
          p := i;
          isRepeat := False;
        end
        else if (i - p) >= maxLen then
        begin
          Count := i - p - 1;
          AStream.WriteBuffer(Count, FBytesPerPixel);
          AStream.WriteBuffer(FBitMap[p * FBytesPerPixel], (i - p) * FBytesPerPixel);
          p := i;
        end;
        last := cur;
      end
      else if not isRepeat then
      begin
        Count := i - p - 2;
        if Count >= 0 then
        begin
          AStream.WriteBuffer(Count, FBytesPerPixel);
          AStream.WriteBuffer(FBitMap[p * FBytesPerPixel], (i - p - 1) * FBytesPerPixel);
          p := i - 1;
        end;
        isRepeat := True;
      end
      else if (i - p) >= maxLen then
      begin
        Count := (i - p - 1) or mask;
        AStream.WriteBuffer(Count, FBytesPerPixel);
        AStream.WriteBuffer(last, FBytesPerPixel);
        p := i;
        isRepeat := False;
      end;
    end;
    Count := Height * Width - 1 - p;
    if isRepeat then
    begin
      Count := Count or mask;
      AStream.WriteBuffer(Count, FBytesPerPixel);
      AStream.WriteBuffer(last, FBytesPerPixel);
    end
    else
    begin
      AStream.WriteBuffer(Count, FBytesPerPixel);
      AStream.WriteBuffer(FBitMap[p * FBytesPerPixel], (Count + 1) * FBytesPerPixel);
    end;
  end;
end;

procedure TBitMapFileData.SetAreaCenter(Area, X, Y: Integer);
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

{ TAreaSizeCalculator }

procedure TAreaSizeCalculator.CalculateAreaSize(X, Y: Integer;
  SizeType: TAreaSizeType; APathMethod: TPathAreaByPoint);
var L, T, B, R: Integer;
begin
  L:= Left;
  R:= Right;
  T:= Top;
  B:= Bottom;
  //вычисляем размеры
  APathMethod(X, Y, SizeType = astOuterWithUnbound, function (cX, cY: Integer; Direct: TDirection): Boolean
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

function TAreaSizeCalculator.GetHeight: Integer;
begin
  Result := Self.Bottom - Self.Top;
end;

function TAreaSizeCalculator.GetWidth: Integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TAreaSizeCalculator.Initialize(AWidth, AHeight: Integer);
begin
  Left:= AWidth + 1;
  Top:= AHeight + 1;
  Right:= -1;
  Bottom:= -1;
end;

end.
