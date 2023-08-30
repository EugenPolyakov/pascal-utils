unit SysUtilsExtensions;

interface

uses System.SysUtils, System.Classes, RecordUtils;

type
  TUCS4Encoding = class (TEncoding)
  strict private
    class var
      FUCS4Encoding: TEncoding;
    class destructor Destroy;
    class function GetUCS4: TEncoding; static;
  protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetCodePage: Cardinal; override;
    function GetEncodingName: string; override;
  public
    class property UCS4: TEncoding read GetUCS4;
    function Clone: TEncoding; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

  TObserversList<T> = class
  protected
    FSubscribers: TListRecord<T>;
  public
    //class constructor Create;
    procedure Subscribe(const AEvent: T);
    procedure Unsubscribe(const AEvent: T);
  end;

  TAction<T1> = procedure (const AValue1: T1) of object;
  TAction<T1, T2> = procedure (const AValue1: T1; const AValue2: T2) of object;
  TAction<T1, T2, T3> = procedure (const AValue1: T1; const AValue2: T2; const AValue3: T3) of object;

  TObserversWithSenderList<T, O> = class (TObserversList<T>)
  strict private
    FSenderObject: O;
    FLockCounter: Integer;
    FRealCallCount: Integer;
  protected
    procedure InnerNotify; virtual; abstract;
  public
    property SenderObject: O read FSenderObject;
    constructor Create(const ASenderObject: O);
    procedure Notify;
    procedure Lock;
    procedure Unlock;
    function IsLocked: Boolean; inline;
  end;

  TObjerversListWithParams<T1> = class (TObserversList<TAction<T1>>)
  public
    procedure Notify(const AValue: T1);
  end;

  TObserversListWithParamsSync<T1> = class(TObserversList<TAction<T1>> )
  private
  protected
    procedure QueueAction(Index: Integer; const AValue: T1);
  public
    procedure Notify(const AValue: T1);
  end;

  TObjerversListWithParams<T1, T2> = class (TObserversList<TAction<T1, T2>>)
  public
    procedure Notify(const AValue1: T1; const AValue2: T2);
  end;

  TObserverListWithParamsSync<T1, T2> = class(TObserversList<TAction<T1, T2>>)
  private
  protected
    procedure QueueAction(Index: Integer; const AValue1: T1; const AValue2: T2);
  public
    procedure Notify(const AValue1: T1; const AValue2: T2);
  end;

  TObjerversListWithParams<T1, T2, T3> = class (TObserversList<TAction<T1, T2, T3>>)
  public
    procedure Notify(const AValue1: T1; const AValue2: T2; const AValue3: T3);
  end;

  TObserverListWithParamsSync<T1, T2, T3> = class(TObserversList<TAction<T1, T2, T3>>)
  private
  protected
    procedure QueueAction(Index: Integer; const AValue1: T1; const AValue2: T2; const AValue3: T3);
  public
    procedure Notify(const AValue1: T1; const AValue2: T2; const AValue3: T3);
  end;

  TObserversListNotifyEvent<T> = class (TObserversWithSenderList<TAction<T>, T>)
  private
  protected
    procedure InnerNotify; override;
  public
  end;

  TObserversListNotifyEvent = class (TObserversWithSenderList<TNotifyEvent, TObject>)
  private
  protected
    procedure InnerNotify; override;
  public
  end;

  EWrapper = class (Exception)
  public
    constructor Create(const Msg: string; AInnerException: Exception);
  end;

const
  BitsInByte = 8;

function SwapRaw(const Value; Size: Integer): UInt64; inline;
procedure SwapAny(var Value; Size: Integer);
procedure SwapInBits(var Value; SizeInBits: Integer); overload; inline;
function SwapBytes(Value: Int64; Size: Integer): Int64; overload; inline;
function IsFirstSurrogateChar(C: WideChar): Boolean; inline;
function IsSecondSurrogateChar(C: WideChar): Boolean; inline;
function SurrogateToUCS4Char(C: PWideChar): UCS4Char; inline;
function UCS4ToSurrogate(C: UCS4Char; Dest: PWideChar): Integer; inline;
procedure AddUCS4ToString(C: UCS4Char; var S: string);

implementation

uses
  System.TypInfo, System.Generics.Defaults;

{$R-}

function IsFirstSurrogateChar(C: WideChar): Boolean;
begin
  Result := (C >= #$D800) and (C < #$DC00);
end;

function IsSecondSurrogateChar(C: WideChar): Boolean;
begin
  Result := (C >= #$DC00) and (C < #$E000);
end;

function SurrogateToUCS4Char(C: PWideChar): UCS4Char;
begin
  if IsFirstSurrogateChar(C^) then
    Result:= (Ord(C[0]) - $D800) shl 10 + Ord(C[1]) + $2400//- $DC00 + $10000
  else
    Result:= UCS4Char(C^);
end;

function UCS4ToSurrogate(C: UCS4Char; Dest: PWideChar): Integer;
begin
  if C > $FFFF then begin
    C:= C - $10000;
    Dest[0]:= WideChar($D800 + C shr 10);
    Dest[1]:= WideChar($DC00 + C and $3FF);
    Result:= 2;
  end else begin
    Dest[0]:= WideChar(C);
    Result:= 1;
  end;
end;

procedure AddUCS4ToString(C: UCS4Char; var S: string);
begin
  if C > $FFFF then begin
    SetLength(S, Length(S) + 2);
    C:= C - $10000;
    S[Length(S) - 1]:= WideChar($D800 + C shr 10);
    S[Length(S)]:= WideChar($DC00 + C and $3FF);
  end else begin
    S:= S + WideChar(C);
  end;
end;

procedure SwapInBits(var Value; SizeInBits: Integer);
var b: array [0..0] of Byte absolute Value;
    i, l: Integer;
    t: Byte;
begin
  l:= (SizeInBits + BitsInByte - 1) div BitsInByte - 1;
  for i := 0 to l div 2 do begin
    t:= b[i];
    b[i]:= b[l - i];
    b[l - i]:= t;
  end;
end;

function SwapBytes(Value: Int64; Size: Integer): Int64; overload;
var i: Integer;
    b: array [0..0] of Byte absolute Value;
begin
  Result:= 0;
  for i := 0 to Size - 1 do
    Int64Rec(Result).Bytes[Size - i - 1]:= Int64Rec(Value).Bytes[i];
end;

function SwapRaw(const Value; Size: Integer): UInt64;
var i: Integer;
    b: array [0..0] of Byte absolute Value;
begin
  Result:= 0;
  for i := 0 to Size - 1 do
    Int64Rec(Result).Bytes[Size - i - 1]:= b[i];
end;

procedure SwapAny(var Value; Size: Integer);
var i: Integer;
    b: array [0..0] of Byte absolute Value;
    z: Byte;
begin
  for i := 0 to Size div 2 - 1 do begin
    z:= b[Size - i - 1];
    b[Size - i - 1]:= b[i];
    b[i]:= z;
  end;
end;

{ TUCS4Encoding }

function TUCS4Encoding.Clone: TEncoding;
begin
  Result:= TUCS4Encoding.Create;
end;

class destructor TUCS4Encoding.Destroy;
begin
  FreeAndNil(FUCS4Encoding);
end;

function TUCS4Encoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
var i, surrogatesCount: Integer;
begin
  surrogatesCount:= 0;
  for i := 0 to CharCount - 1 do
    if IsFirstSurrogateChar(Chars[i]) then
      Inc(surrogatesCount);
  Result:= (CharCount + surrogatesCount) * 4;
end;

function TUCS4Encoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte;
  ByteCount: Integer): Integer;
var i: Integer;
begin
  i:= 0;
  Result:= 0;
  while (i < CharCount) and (Result < ByteCount - 4) do begin
    PUCS4Char(@Bytes[Result])^:= SurrogateToUCS4Char(@Chars[i]);
    if IsFirstSurrogateChar(Chars[i]) then
      Inc(i);
    Inc(i);
    Inc(Result, 4);
  end;
end;

function TUCS4Encoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
var i: Integer;
begin
  Result:= ByteCount div 4;
  for i := 0 to Result - 1 do
    if PUCS4Char(@Bytes[i * 4])^ > $FFFF then
      Inc(Result);
end;

function TUCS4Encoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar;
  CharCount: Integer): Integer;
var i: Integer;
begin
  i:= 0;
  Result:= 0;
  while (Result < CharCount) and (i <= ByteCount - 4) do begin
    Inc(Result, UCS4ToSurrogate(PUCS4Char(@Bytes[i])^, @Chars[Result]));
    Inc(i, 4);
  end;
end;

function TUCS4Encoding.GetCodePage: Cardinal;
begin
  Result:= 12000;
end;

function TUCS4Encoding.GetEncodingName: string;
begin
{$IFDEF MSWINDOWS}
  Result := '12000  (UTF-32LE)'; // do not localize
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Result := 'Unicode (UTF-32LE)'; // do not localize
{$ENDIF POSIX}
end;

function TUCS4Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result:= CharCount * 4;
end;

function TUCS4Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result:= ByteCount div 2;
end;

function TUCS4Encoding.GetPreamble: TBytes;
begin
  Result:= TBytes.Create($FF, $FE, $00, $00);
end;

class function TUCS4Encoding.GetUCS4: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FUCS4Encoding = nil then
  begin
    LEncoding := TUCS4Encoding.Create;
    if AtomicCmpExchange(Pointer(FUCS4Encoding), Pointer(LEncoding), nil) <> nil then
      LEncoding.Free;
{$IFDEF AUTOREFCOUNT}
    FUCS4Encoding.__ObjAddRef;
{$ENDIF AUTOREFCOUNT}
  end;
  Result := FUCS4Encoding;
end;

{ TObserversList<T, O> }

{class constructor TObserversList<T, O>.Create;
begin
  if PTypeInfo(TypeInfo(T)).Kind <> tkMethod then
    raise Exception.Create('TObserversList wrong event type.');
end;}

procedure TObserversList<T>.Subscribe(const AEvent: T);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i := 0 to FSubscribers.Count - 1 do
      if TEqualityComparer<T>.Default.Equals(FSubscribers[i], AEvent) then
        Exit;

    FSubscribers.Add(AEvent);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TObserversList<T>.Unsubscribe(const AEvent: T);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i := 0 to FSubscribers.Count - 1 do
      if TEqualityComparer<T>.Default.Equals(FSubscribers[i], AEvent) then begin
        FSubscribers.Delete(i);
        Exit;
      end;
  finally
    TMonitor.Exit(Self);
  end;
end;

{ TObserversListNotifyEvent }

procedure TObserversListNotifyEvent.InnerNotify;
var
  i: Integer;
  arr: TArray<TNotifyEvent>;
begin
  TMonitor.Enter(Self);
  try
    arr:= FSubscribers.ToArray;
  finally
    TMonitor.Exit(Self);
  end;
  for i := 0 to High(arr) do
    arr[i](SenderObject);
end;

{ TObserversListNotifyEvent<T> }

procedure TObserversListNotifyEvent<T>.InnerNotify;
var
  i: Integer;
  func: TAction<T>;
  arr: TArray<TAction<T>>;
begin
  TMonitor.Enter(Self);
  try
    arr:= FSubscribers.ToArray;
  finally
    TMonitor.Exit(Self);
  end;
  for i := 0 to High(arr) do begin
    func:= arr[i];
    func(SenderObject);
  end;
end;

{ TObserversWithSenderList<T, O> }

constructor TObserversWithSenderList<T, O>.Create(const ASenderObject: O);
begin
  FSenderObject:= ASenderObject;
end;

function TObserversWithSenderList<T, O>.IsLocked: Boolean;
begin
  Result:= FLockCounter <> 0;
end;

procedure TObserversWithSenderList<T, O>.Lock;
begin
  AtomicIncrement(FLockCounter);
end;

procedure TObserversWithSenderList<T, O>.Notify;
begin
  //not thread safe
  if not IsLocked then
    InnerNotify
  else
    AtomicIncrement(FRealCallCount);
end;

procedure TObserversWithSenderList<T, O>.Unlock;
begin
  if AtomicDecrement(FLockCounter) = 0 then begin
    if FRealCallCount > 0 then
      InnerNotify;
    FRealCallCount:= 0;
  end;
end;

{ EWrapper }

constructor EWrapper.Create(const Msg: string; AInnerException: Exception);
var x: Pointer;
begin
  inherited Create(Msg);
  x:= @InnerException;
  Pointer(x^):= AInnerException;
end;

{ TObjerversListWithParams<T1, T2, T3> }

procedure TObjerversListWithParams<T1, T2, T3>.Notify(const AValue1: T1; const AValue2: T2; const AValue3: T3);
var
  i: Integer;
  func: TAction<T1, T2, T3>;
begin
  for i := 0 to FSubscribers.Count - 1 do begin
    func:= FSubscribers[i];
    func(AValue1, AValue2, AValue3);
  end;
end;

{ TObjerversListWithParams<T1, T2> }

procedure TObjerversListWithParams<T1, T2>.Notify(const AValue1: T1; const AValue2: T2);
var
  i: Integer;
  func: TAction<T1, T2>;
begin
  for i := 0 to FSubscribers.Count - 1 do begin
    func:= FSubscribers[i];
    func(AValue1, AValue2);
  end;
end;

{ TObjerversListWithParams<T1> }

procedure TObjerversListWithParams<T1>.Notify(const AValue: T1);
var
  i: Integer;
  func: TAction<T1>;
begin
  for i := 0 to FSubscribers.Count - 1 do begin
    func:= FSubscribers[i];
    func(AValue);
  end;
end;

{ TObserversListWithParamsSync<T1> }

procedure TObserversListWithParamsSync<T1>.Notify(const AValue: T1);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i:= 0 to FSubscribers.Count - 1 do
      QueueAction(i, AValue);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TObserversListWithParamsSync<T1>.QueueAction(Index: Integer; const AValue: T1);
var
  func: TAction<T1>;
begin
  func:= FSubscribers[index];
  TThread.Queue(nil,
    procedure
    begin
      func(AValue);
    end);
end;

{ TObserverListWithParamsSync<T1, T2> }

procedure TObserverListWithParamsSync<T1, T2>.Notify(const AValue1: T1; const AValue2: T2);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i:= 0 to FSubscribers.Count - 1 do
      QueueAction(i, AValue1, AValue2);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TObserverListWithParamsSync<T1, T2>.QueueAction(Index: Integer; const AValue1: T1; const AValue2: T2);
var
  func: TAction<T1, T2>;
begin
  func:= FSubscribers[index];
  TThread.Queue(nil,
    procedure
    begin
      func(AValue1, AValue2);
    end);
end;

{ TObserverListWithParamsSync<T1, T2, T3> }

procedure TObserverListWithParamsSync<T1, T2, T3>.Notify(const AValue1: T1; const AValue2: T2; const AValue3: T3);
var
  i: Integer;
begin
  TMonitor.Enter(Self);
  try
    for i:= 0 to FSubscribers.Count - 1 do
      QueueAction(i, AValue1, AValue2, AValue3);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TObserverListWithParamsSync<T1, T2, T3>.QueueAction(Index: Integer; const AValue1: T1; const AValue2: T2;
  const AValue3: T3);
var
  func: TAction<T1, T2, T3>;
begin
  func:= FSubscribers[index];
  TThread.Queue(nil,
    procedure
    begin
      func(AValue1, AValue2, AValue3);
    end);
end;

end.
