unit StreamExtensions;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TStreamAgregator = class (TStream)
  private
    FStreams: array of TStream;
    FMainStream: TStream;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AMainStream: TStream; const AStreams: array of TStream);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
  end;

  TStreamCover = class (TStream)
  private
    FOwnStream: TStream;
    FOwned: Boolean;
    FBeginPosition: Int64;
  protected
    FCurrentOffset: Int64;
    property BeginPosition: Int64 read FBeginPosition;
    property OwnStream: TStream read FOwnStream;
    function GetSize: Int64; override;
  public
    procedure Assign(AStream: TStreamCover); virtual;
    procedure ReconectStream(AStream: TStream; AOwned: Boolean = False); virtual;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure FixPosition; inline;
    function Read(var Buffer; Count: Longint): Longint; override;
    constructor Create(AStream: TStream; AOwned: Boolean = False);
    destructor Destroy; override;
  end;

  TPartialStream = class (TStreamCover)
  private
    FBoundSize: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AMainStream: TStream; ASize: Int64; AOwned: Boolean = False);
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  TBufferedStream = class abstract (TStreamCover)
  private
    FFlatBuffer: Boolean;
  protected
    FBuffer: PByte;
    FBufferSize: Integer;
    //Actual buffer filling
    //for read - tail not readed buffer
    //for write - written count
    FBufferFill: Integer;
    constructor CreateDefault(AStream: TStream; ABuffer: PByte; ABufferSize: LongWord; AOwned: Boolean); virtual;
    class function CreateFlatBlock(AStream: TStream; ABufferSize: LongWord = 4096; AOwned: Boolean = True): TBufferedStream;
  public
    property OwnStream;
    property CurrentBufferSize: Integer read FBufferFill;
    constructor Create(AStream: TStream; AOwned: Boolean = True; ABufferSize: LongWord = 4096);
    destructor Destroy; override;
  end;

  TBufferedReader = class (TBufferedStream)
  protected
    FBufferCount: Integer; //Current buffer filling
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    class function CreateFlatBlock(AStream: TStream; AOwned: Boolean = True; ABufferSize: LongWord = 4096): TBufferedReader; inline;
  end;

  TBufferedWriter = class (TBufferedStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Flash; inline;
    destructor Destroy; override;
    class function CreateFlatBlock(AStream: TStream; AOwned: Boolean = True; ABufferSize: LongWord = 4096): TBufferedWriter; inline;
  end;

  TBitStream = class (TBufferedReader)
  private
    FNeedSwap: Boolean;
    FStartOffset: Int64;
  protected
    //last bits count in FLastByte
    FFillBitsCount: ShortInt;
    FLastByte: Byte;
    function GetSize: Int64; override;
    function ReadAndAlign(var Buffer; BytesCount, BitsCount: Integer): Integer; overload; virtual; abstract;
    function ReadBytes(var Buffer; Count: Integer): Integer;
  public
    procedure AlignToByte; inline;
    function Peek(SizeInBytes: Integer = 1): UInt64;
    function PeekBits(Size: Integer): UInt64;
    function Read(var Buffer; Count: Integer): Integer; override;
    function ReadBits(BitsCount: Integer): UInt64; overload;
    function ReadBits(var Value; BitsCount: Integer): Integer; overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    procedure SeekBits(const Offset: Int64; Origin: TSeekOrigin);
    property FillBitsCount: ShortInt read FFillBitsCount;
    property NeedSwap: Boolean read FNeedSwap write FNeedSwap;
  end;

  //most significant bit
  // 00001010 00
  //          ^
  // 00001010 00111111
  TMSBBitStream = class (TBitStream)
  protected
    function ReadAndAlign(var Buffer; BytesCount, BitsCount: Integer): Integer; override;
    constructor CreateDefault(AStream: TStream; ABuffer: PByte; ABufferSize: LongWord; AOwned: Boolean); override;
  public
    class function CreateFlatBlock(AStream: TStream; AOwned: Boolean = True; ABufferSize: LongWord = 4096): TMSBBitStream; inline;
  end;

  //least significant bit
  // read low bits, add to high
  // 00 00001010
  //  ^--------------
  // 00001010 11111100
  TLSBBitStream = class (TBitStream)
  protected
    function ReadAndAlign(var Buffer; BytesCount, BitsCount: Integer): Integer; override;
    constructor CreateDefault(AStream: TStream; ABuffer: PByte; ABufferSize: LongWord; AOwned: Boolean); override;
  public
    class function CreateFlatBlock(AStream: TStream; AOwned: Boolean = True; ABufferSize: LongWord = 4096): TLSBBitStream; inline;
  end;

  //light stream only for read
  TTapeReader = class (TObject)
  private
  protected
    FEoF: Boolean;
  public
    function Peek: UCS4Char; virtual;
    function Read: UCS4Char;
    //must set EoF
    function Skip: Boolean; virtual; abstract;
    property EoF: Boolean read FEoF;
  end;

  TStringTape = class (TTapeReader)
  private
    FCurrentPosition: Integer;
    FString: string;
  protected
  public
    constructor Create(const AString: string);
    function Peek: UCS4Char; override;
    function Skip: Boolean; override;
  end;

  TStreamTape = class (TTapeReader)
  private
    FOwner: Boolean;
    FStream: TStream;
    FEncoding: TEncoding;
    FCharBuffer: array [0..4095] of Char;
    FCharCount,            //buffer filling count
    FCharPosition: Integer;//current char index
    FBuffer: array [0..4095] of Byte;
    FCount,
    FOffset: Integer;
    FEndOfStream: Boolean;
    procedure SetEncoding(Value: TEncoding);
  protected
    procedure UpdateBuffer;
  public
    constructor Create(AStream: TStream; AEncoding: TEncoding = nil; AOwner: Boolean = True);
    destructor Destroy; override;
    function Peek: UCS4Char; override;
    function Skip: Boolean; override;
    property EndOfStream: Boolean read FEndOfStream;
    property Encoding: TEncoding read FEncoding write SetEncoding;
  end;

  TStreamHelper = class helper for TStream
    function ReadByte: Byte;
  end;

  TLR1 = class;

  TLR1Action = procedure (Self: TLR1; CurrentKey: UCS4Char; CurrentState: Integer);

  TKeyTransition = record
    BeginKey, EndKey: UCS4Char;
    NextState: Integer;
    Action: TLR1Action;
  end;

  TKeyTransitions = array [0..MaxInt div SizeOf(TKeyTransition) - 1] of TKeyTransition;
  PKeyTransitions = ^TKeyTransitions;

  TMagazineTransition = record
    BeginMagazine, EndMagazine: UCS4Char;
    KeyTransitions: PKeyTransitions;
    KeyTransitionsLength: Integer;
  end;

  TMagazineTransitions = array [0..MaxInt div SizeOf(TMagazineTransition) - 1] of TMagazineTransition;
  PMagazineTransitions = ^TMagazineTransitions;

  TStateTransition = record
    MagazineTransitions: PMagazineTransitions;
    MagazineTransitionsLength: Integer;
  end;

  TStateTransitions = array [0..MaxInt div SizeOf(TStateTransition) - 1] of TStateTransition;
  PStateTransitions = ^TStateTransitions;

  TLR1 = class
  private
    FStates: PStateTransitions;
    FLength: Integer;
    FMagazine: TList<UCS4Char>;
    FTape: TTapeReader;
    FCurrentState: Integer;
    FCurrentString: string;
  protected
    property CurrentState: Integer read FCurrentState write FCurrentState;
    property CurrentString: string read FCurrentString;
    procedure PushKey(AKey: UCS4Char);
    procedure ClearCurrentString;
    procedure ToNextKey;
    function IsTapeEnd: Boolean;
    function GetKey: UCS4Char;
    function GetState: UCS4Char;
    function PopState: UCS4Char;
    function IsEmptyMagazine: Boolean;
    procedure PushState(State: UCS4Char);
    function CheckMagazine(const AValue: string): Boolean;
    function GetMagazineAsString(BeginPosition: Integer = 0): string;
    function PopMagazineAsString(BeginPosition: Integer = 0): string;
    function GetLastMagazine(const Values: array of UCS4Char): Integer;
    function IsMagazineEqual(Value: UCS4Char): Boolean;
    procedure ClearMagazine;

    class procedure RepeatAtNewState(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure RepeatAndBack(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure PushAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure PushAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ExchangeAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure SkipAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure SkipAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure SkipAndBack(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndRepeatAtNewState(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndPushAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndPushAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndRepeat(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndSkipAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ClearAndSkipAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure PopAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure BreakAction(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ReadAndContinue(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ReadAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
    class procedure ReadAndBack(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer); static;
  public
    constructor Create(AStates: PStateTransitions; ALength: Integer);
    procedure RunParse(ATape: TTapeReader; BeginState: Integer);
    destructor Destroy; override;
  end;

  TSimpleRegEx = class (TLR1)
  private
  protected
  public
  end;

  TPublicEncoding = class (TEncoding)
  public
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override; abstract;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override; abstract;
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override; abstract;
    property MaxCharSize: Integer read FMaxCharSize;
  end;

const
  _end = UCS4Char(-1);

implementation

uses SysUtilsExtensions;

{ TBufferedStream }

constructor TBufferedStream.Create(AStream: TStream; AOwned: Boolean; ABufferSize: LongWord);
var Buf: PByte;
begin
  GetMem(Buf, ABufferSize);
  CreateDefault(AStream, Buf, ABufferSize, AOwned);
  FFlatBuffer:= False;
end;

constructor TBufferedStream.CreateDefault(AStream: TStream; ABuffer: PByte;
  ABufferSize: LongWord; AOwned: Boolean);
begin
  inherited Create(AStream, AOwned);
  FBufferSize:= ABufferSize;
  FBuffer:= ABuffer;
  FFlatBuffer:= True;
end;

class function TBufferedStream.CreateFlatBlock(AStream: TStream;
  ABufferSize: LongWord; AOwned: Boolean): TBufferedStream;
begin
  GetMem(Pointer(Result), InstanceSize + ABufferSize);
  InitInstance(Result);
  Result.CreateDefault(AStream, @PByte(Result)[InstanceSize], ABufferSize, AOwned);
end;

destructor TBufferedStream.Destroy;
begin
  if not FFlatBuffer then
    FreeMem(FBuffer);
  inherited;
end;

{ TStreamAgregator }

constructor TStreamAgregator.Create(AMainStream: TStream;
  const AStreams: array of TStream);
begin
  FMainStream:= AMainStream;
  SetLength(FStreams, Length(AStreams));
  Move(AStreams[0], FStreams[0], Length(AStreams) * SizeOf(TStream));
end;

destructor TStreamAgregator.Destroy;
var i: Integer;
begin
  FMainStream.Free;
  for i := 0 to High(FStreams) do
    FStreams[i].Free;
  inherited;
end;

function TStreamAgregator.GetSize: Int64;
begin
  Result:= FMainStream.Size;
end;

function TStreamAgregator.Read(var Buffer; Count: Integer): Longint;
begin
  Result:= FMainStream.Read(Buffer, Count);
end;

function TStreamAgregator.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result:= FMainStream.Seek(Offset, Origin);
end;

function TStreamAgregator.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= FMainStream.Seek(Offset, Origin);
end;

procedure TStreamAgregator.SetSize(const NewSize: Int64);
begin
  FMainStream.Size:= NewSize;
end;

function TStreamAgregator.Write(const Buffer; Count: Integer): Longint;
begin
  Result:= FMainStream.Write(Buffer, Count);
end;

{ TStreamHelper }

function TStreamHelper.ReadByte: Byte;
begin
  ReadBuffer(Result, 1);
end;

{ TPartialStream }

constructor TPartialStream.Create(AMainStream: TStream; ASize: Int64; AOwned: Boolean);
begin
  inherited Create(AMainStream, AOwned);
  FBoundSize:= ASize;
end;

function TPartialStream.GetSize: Int64;
begin
  Result:= FBoundSize;
end;

function TPartialStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count > FBoundSize - FCurrentOffset then
    Count:= FBoundSize - FCurrentOffset;

  Result:= inherited Read(Buffer, Count);
end;

function TPartialStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: Result:= Offset;
    soCurrent: Result:= FCurrentOffset + Offset;
  else//soEnd:
    Result:= FBoundSize + Offset;
  end;

  if (Result > FBoundSize) or (Result < 0) then begin
    Result:= FCurrentOffset;
  end else
    FCurrentOffset:= Result;
end;

{ TBufferedWriter }
class function TBufferedWriter.CreateFlatBlock(AStream: TStream; AOwned: Boolean;
  ABufferSize: LongWord): TBufferedWriter;
begin
  Result:= TBufferedWriter(inherited CreateFlatBlock(AStream, ABufferSize, AOwned));
end;

destructor TBufferedWriter.Destroy;
begin
  Flash;
  inherited;
end;

procedure TBufferedWriter.Flash;
begin
  if FBufferFill > 0 then begin
    OwnStream.Write(FBuffer^, FBufferFill);
    FBufferFill:= 0;
  end;
end;

function TBufferedWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Exit(OwnStream.Seek(Offset, Origin) + FBufferFill);
  Flash;
  Result:= OwnStream.Seek(Offset, Origin);
end;

function TBufferedWriter.Write(const Buffer; Count: Integer): Longint;
var toBuf: Integer;
begin
  if (FBufferFill = 0) and (Count > FBufferSize) then
    Result:= OwnStream.Write(Buffer, Count)
  else if Count > FBufferSize * 2 - FBufferFill then begin
    Flash;
    Result:= OwnStream.Write(Buffer, Count);
  end else begin
    if Count > FBufferSize - FBufferFill then
      toBuf:= FBufferSize - FBufferFill
    else
      toBuf:= Count;
    Move(Buffer, FBuffer[FBufferFill], toBuf);
    Inc(FBufferFill, toBuf);

    if FBufferFill = FBufferSize then begin
      OwnStream.Write(FBuffer^, FBufferFill);
      FBufferFill:= 0;
    end;

    if toBuf <> Count then begin
      Move(PAnsiChar(@Buffer)[toBuf], FBuffer[FBufferFill], Count - toBuf);
      Inc(FBufferFill, Count - toBuf);
    end;
    Result:= Count;
  end;
end;

{ TBufferedReader }

class function TBufferedReader.CreateFlatBlock(AStream: TStream;
  AOwned: Boolean; ABufferSize: LongWord): TBufferedReader;
begin
  Result:= TBufferedReader(inherited CreateFlatBlock(AStream, ABufferSize, AOwned));
end;

function TBufferedReader.Read(var Buffer; Count: Integer): Longint;
var SBuffer: array [0..0] of Byte absolute Buffer;
begin
  if Count < FBufferFill then
    Result:= Count
  else
    Result:= FBufferFill;

  if Result > 0 then begin
    Move(FBuffer[FBufferCount - FBufferFill], SBuffer[0], Result);
    Dec(FBufferFill, Result);
  end;
  Dec(Count, Result);
  if Count > 0 then begin
    if Count > FBufferSize then begin
      Inc(Result, inherited Read(SBuffer[Result], Count));
    end else begin
      FBufferFill:= inherited Read(FBuffer[0], FBufferSize);
      FBufferCount:= FBufferFill;
      if FBufferFill < Count then
        Count:= FBufferFill;
      Move(FBuffer[0], SBuffer[Result], Count);
      Dec(FBufferFill, Count);
      Inc(Result, Count);
    end;
  end;
end;

function TBufferedReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var tmp, nextOfs: Int64;
begin
  case Origin of
    soBeginning: begin
      if (FCurrentOffset - FBufferCount <= Offset) and (FCurrentOffset > Offset) then begin
        Origin:= soCurrent;
        tmp:= Offset - FCurrentOffset + FBufferFill;
      end else
        tmp:= Offset + BeginPosition;
    end;
    soCurrent: tmp:= Offset;
  else//soEnd: begin
      tmp:= OwnStream.Size - FBeginPosition + Offset;
      if (FCurrentOffset - FBufferCount <= tmp) and (FCurrentOffset > tmp) then begin
        Origin:= soCurrent;
        Dec(tmp, FCurrentOffset - FBufferFill);
      end else
        tmp:= Offset;
  end;
  if Origin <> soCurrent then begin
    FBufferFill:= 0;
    Result:= inherited Seek(tmp, Origin);
  end else begin
    if tmp = 0 then
      Exit(FCurrentOffset - FBufferFill);

    nextOfs:= FBufferFill - tmp;
    if (nextOfs > 0) and (nextOfs <= FBufferCount) then begin
      FBufferFill:= nextOfs;
      Result:= FCurrentOffset - FBufferFill;
    end else begin
      Dec(FCurrentOffset, FBufferFill);
      FBufferFill:= 0;
      Result:= inherited Seek(tmp, Origin);
    end;
  end;
end;

{ TStreamCover }

procedure TStreamCover.Assign(AStream: TStreamCover);
begin
  FOwnStream:= AStream.FOwnStream;
  FOwned:= AStream.FOwned;
  FBeginPosition:= AStream.FBeginPosition;
  FCurrentOffset:= AStream.FCurrentOffset;
end;

constructor TStreamCover.Create(AStream: TStream; AOwned: Boolean);
begin
  ReconectStream(AStream, AOwned);
end;

destructor TStreamCover.Destroy;
begin
  if FOwned then
    FOwnStream.Free;

  inherited;
end;

procedure TStreamCover.FixPosition;
begin
  if FOwnStream.Position <> FCurrentOffset + FBeginPosition then
    FOwnStream.Position:= FCurrentOffset + FBeginPosition;
end;

function TStreamCover.GetSize: Int64;
begin
  Result:= OwnStream.Size - FBeginPosition;
end;

function TStreamCover.Read(var Buffer; Count: Integer): Longint;
begin
  FixPosition;
  Result:= OwnStream.Read(Buffer, Count);
  Inc(FCurrentOffset, Result);
end;

procedure TStreamCover.ReconectStream(AStream: TStream; AOwned: Boolean);
begin
  FOwnStream:= AStream;
  FOwned:= AOwned;
  FBeginPosition:= AStream.Position;
  FCurrentOffset:= 0;
end;

function TStreamCover.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
label soEndLabel;
begin
  case Origin of
    //We can do deferred seek cause know position
    //soBeginning: FCurrentOffset:= OwnStream.Seek(Offset + FBeginPosition, soBeginning) - FBeginPosition;
    soBeginning:
      if Offset < 0 then
        FCurrentOffset:= 0
      else
        FCurrentOffset:= Offset;
    soCurrent:
      if Offset > 0 then begin
        FixPosition;
        goto soEndLabel;
      end else begin
        Inc(FCurrentOffset, Offset);
        if FCurrentOffset < 0 then
          FCurrentOffset:= 0;
      end;
    soEnd:soEndLabel: FCurrentOffset:= OwnStream.Seek(Offset, Origin) - FBeginPosition;
  end;

  Result:= FCurrentOffset;
end;

{ TMSBBitStream }
constructor TMSBBitStream.CreateDefault(AStream: TStream; ABuffer: PByte;
  ABufferSize: LongWord; AOwned: Boolean);
begin
  inherited;
  NeedSwap:= True;
end;

class function TMSBBitStream.CreateFlatBlock(AStream: TStream; AOwned: Boolean;
  ABufferSize: LongWord): TMSBBitStream;
begin
  Result:= TMSBBitStream(inherited CreateFlatBlock(AStream, AOwned, ABufferSize));
end;

function TMSBBitStream.ReadAndAlign(var Buffer; BytesCount,
  BitsCount: Integer): Integer;
var SBuffer: array [0..0] of Byte absolute Buffer;
    i, AlignBits: Integer;
    tmp: Byte;
    Last: Word;
begin
  if (BytesCount = 0) and (BitsCount <= FFillBitsCount) then begin
    SBuffer[0]:= FLastByte shr (FFillBitsCount - BitsCount) and ($FF shr (BitsInByte - BitsCount));
    Dec(FFillBitsCount, BitsCount);
    FLastByte:= FLastByte and not (-1 shl FFillBitsCount);
    Result:= 0;
  end else begin
    AlignBits:= (BitsInByte - BitsCount) mod BitsInByte;
    if BitsCount <> 0 then
      Inc(BytesCount);

    Result:= ReadBytes(SBuffer[0], BytesCount);

    if (FFillBitsCount <> 0) or (AlignBits <> 0) then begin
      Last:= FLastByte;
      for i:= 0 to Result - 1 do begin
        tmp:= SBuffer[i];
        SBuffer[i]:= ((tmp shr FFillBitsCount) + Last shl (BitsInByte - FFillBitsCount)) shr AlignBits;
        Last:= Last shl BitsInByte + tmp;
      end;
      Inc(FFillBitsCount, AlignBits);
      if FFillBitsCount >= BitsInByte then begin
        Inc(FBufferFill);
        FBuffer[FBufferCount - FBufferFill]:= Byte(Last);// and not ($FF shl FFillBitsCount)
        Last:= Last shr BitsInByte;
        FFillBitsCount:= FFillBitsCount mod BitsInByte;
        Dec(Result);
      end;
      FLastByte:= Byte(Last) and not (-1 shl FFillBitsCount);
    end;
  end;
end;

{ TBitStream }

procedure TBitStream.AlignToByte;
begin
  FLastByte:= 0;
  FFillBitsCount:= 0;
end;

function TBitStream.GetSize: Int64;
begin
  Result:= OwnStream.Size - FStartOffset;
end;

function TBitStream.Peek(SizeInBytes: Integer): UInt64;
var Readed: Integer;
begin
  if SizeInBytes > SizeOf(Result) then
    raise Exception.Create('Size not supported.');
  {if FBufferFill < SizeInBytes then begin
    Move(FBuffer[SizeOf(FBuffer) - FBufferFill], FBuffer[0], FBufferFill);
    Inc(FBufferFill, FOwner.Read(FBuffer[FBufferFill], SizeOf(FBuffer) - FBufferFill));
  end;
  Result:= PCardinal(@FBuffer[SizeOf(FBuffer) - FBufferFill])^ and (-1 shr (SizeInBytes * 8));}
  Result:= 0;
  Readed:= ReadAndAlign(Result, SizeInBytes, 0);
  Seek(-Readed, soCurrent);
  if Readed <> SizeInBytes then
    raise Exception.Create('End of buffer');
end;

function TBitStream.PeekBits(Size: Integer): UInt64;
var Readed: Integer;
begin
  if Size > SizeOf(Result) * BitsInByte then
    raise Exception.Create('Size not supported.');
  Result:= 0;
  readed:= ReadBits(Result, Size);
  SeekBits(-Readed, soCurrent);
  {if Readed <> Size then
    raise Exception.Create('End of buffer');}
end;

function TBitStream.Read(var Buffer; Count: Integer): Integer;
begin
  Result:= ReadAndAlign(Buffer, Count, 0);
end;

function TBitStream.ReadBits(BitsCount: Integer): UInt64;
var i: Integer;
begin
  if BitsCount > SizeOf(Result) * BitsInByte then
    raise Exception.Create('Size not supported.');
  Result:= 0;
  i:= ReadBits(Result, BitsCount);
  if i <> BitsCount then
    raise Exception.Create('Unexpected end of buffer');
end;

function TBitStream.ReadBits(var Value; BitsCount: Integer): Integer;
var i: Integer;
begin
  i:= FFillBitsCount;
  Result:= ReadAndAlign(Value, BitsCount div BitsInByte, BitsCount mod BitsInByte) * BitsInByte + i - FFillBitsCount;
  if FNeedSwap then
    SwapInBits(Value, BitsCount);
end;

function TBitStream.ReadBytes(var Buffer; Count: Integer): Integer;
begin
  Result:= inherited Read(Buffer, Count);
end;

function TBitStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var tmp: Int64;
    needBits: Integer;
begin
  tmp:= Offset;
  case Origin of
    soBeginning: AlignToByte;
    soCurrent: begin
      if Offset = 0 then
        Exit(FCurrentOffset - FBufferFill);
      if FFillBitsCount <> 0 then
        Dec(tmp);
    end;
    soEnd: AlignToByte;
  end;
  Result:= inherited Seek(tmp, Origin);

  if FFillBitsCount <> 0 then begin
    needBits:= FFillBitsCount;
    AlignToByte;
    ReadAndAlign(needBits, 0, BitsInByte - needBits);
  end;
end;

procedure TBitStream.SeekBits(const Offset: Int64; Origin: TSeekOrigin);
var ByteOffset: Int64;
    BitsCount: Integer;
begin
  if Origin <> soCurrent then
    AlignToByte;

  ByteOffset:= Offset div BitsInByte;
  BitsCount:= FFillBitsCount - Offset mod BitsInByte;
  if Offset < 0 then begin
    Dec(ByteOffset, (BitsCount + BitsInByte - 1) div BitsInByte);
    if BitsCount >= BitsInByte then
      Dec(BitsCount, BitsInByte);
  end else begin
    if BitsCount < 0 then
      Inc(BitsCount, BitsInByte);
  end;
  AlignToByte;
  Seek(ByteOffset, Origin);
  if BitsCount <> 0 then
    ReadAndAlign(BitsCount, 0, BitsInByte - BitsCount);
end;

{ TLSBBitStream }

constructor TLSBBitStream.CreateDefault(AStream: TStream; ABuffer: PByte;
  ABufferSize: LongWord; AOwned: Boolean);
begin
  inherited;
  NeedSwap:= False;
end;

class function TLSBBitStream.CreateFlatBlock(AStream: TStream; AOwned: Boolean;
  ABufferSize: LongWord): TLSBBitStream;
begin
  Result:= TLSBBitStream(inherited CreateFlatBlock(AStream, AOwned, ABufferSize));
end;

function TLSBBitStream.ReadAndAlign(var Buffer; BytesCount,
  BitsCount: Integer): Integer;
var SBuffer: array [0..1] of Byte absolute Buffer;
    i, readed: Integer;
    tmp, Last, ofs: Byte;
begin
  {if (BytesCount = 0) and (BitsCount = 0) then
    Exit(0); }
  if (BytesCount = 0) and (BitsCount <= FFillBitsCount) then begin
    SBuffer[0]:= FLastByte and (1 shl BitsCount - 1);
    Dec(FFillBitsCount, BitsCount);
    FLastByte:= FLastByte shr BitsCount;
    Result:= 0;
  end else begin
    if BitsCount > FFillBitsCount then
      Inc(BytesCount);

    Result:= ReadBytes(SBuffer[0], BytesCount);
    readed:= Result;

    if (FFillBitsCount <> 0) or (BitsCount <> 0) then begin
      if (BitsCount < FFillBitsCount) and (BitsCount <> 0) then begin
        SBuffer[readed]:= 0;
        Inc(readed);
      end;
      Last:= 0;
      ofs:= BitsInByte - FFillBitsCount;
      tmp:= FLastByte shl ofs;
      for i:= 0 to readed - 1 do begin
        Last:= tmp shr ofs;
        tmp:= SBuffer[i];
        SBuffer[i]:= (SBuffer[i] shl FFillBitsCount) + Last;
      end;
      if (BitsCount <> 0) then begin
        SBuffer[readed - 1]:= SBuffer[readed - 1] and ((1 shl BitsCount) - 1);
        Dec(FFillBitsCount, BitsCount);
        if FFillBitsCount < 0 then
          Inc(FFillBitsCount, BitsInByte);
        Last:= tmp shr ((BitsInByte - FFillBitsCount) mod BitsInByte) + Last shr BitsCount;
      end else
        Last:= tmp shr ofs;
      FLastByte:= Last;
    end;
  end;
end;

{ TLR1 }

class procedure TLR1.BreakAction(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer);
begin
  Self.CurrentState:= -1;
end;

function TLR1.CheckMagazine(const AValue: string): Boolean;
var CharCount, i, j: Integer;
    Item: UCS4Char;
begin
  CharCount:= 0;
  for i := Low(AValue) to High(AValue) do
    if not IsFirstSurrogateChar(AValue[i]) then
      Inc(CharCount);

  if FMagazine.Count < CharCount then
    Exit(False);

  j:= 0;
  for i := FMagazine.Count - CharCount to FMagazine.Count - 1 do begin
    Item:= SurrogateToUCS4Char(@PChar(Pointer(AValue))[j]);
    if IsFirstSurrogateChar(AValue[j + 1]) then
      Inc(j);
    Inc(j);
    if FMagazine[i] <> Item then
      Exit(False);
  end;
  Result:= True;
end;

class procedure TLR1.ClearAndPushAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
  Self.PushState(ACurrentKey);
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.ClearAndPushAndGo(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
  Self.PushState(ACurrentKey);
  Self.ToNextKey;
end;

class procedure TLR1.ClearAndRepeat(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.ClearAndRepeatAtNewState(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
end;

class procedure TLR1.ClearAndSkipAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.ClearAndSkipAndGo(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ClearMagazine;
  Self.ToNextKey;
end;

procedure TLR1.ClearCurrentString;
begin
  FCurrentString:= '';
end;

procedure TLR1.ClearMagazine;
begin
  FMagazine.Clear;
end;

constructor TLR1.Create(AStates: PStateTransitions; ALength: Integer);
begin
  FStates:= AStates;
  FLength:= ALength;
  FMagazine:= TList<UCS4Char>.Create;
end;

destructor TLR1.Destroy;
begin
  FMagazine.Free;
  inherited;
end;

class procedure TLR1.ExchangeAndGo(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PopState;
  Self.PushState(ACurrentKey);
  Self.ToNextKey;
end;

function TLR1.GetKey: UCS4Char;
begin
  Result:= FTape.Peek;
end;

function TLR1.GetLastMagazine(const Values: array of UCS4Char): Integer;
var
  i: Integer;
  j: Integer;
begin
  for i := FMagazine.Count - 1 downto 0 do
    for j := Low(Values) to High(Values) do
      if FMagazine[i] = Values[j] then
        Exit(i);
  Result:= -1;
end;

function TLR1.GetMagazineAsString(BeginPosition: Integer): string;
var len: Integer;
begin
  len:= TPublicEncoding(TUCS4Encoding.UCS4).GetCharCount(PByte(@FMagazine.List[BeginPosition]), FMagazine.Count * 4);
  SetLength(Result, len);
  TPublicEncoding(TUCS4Encoding.UCS4).GetChars(PByte(@FMagazine.List[BeginPosition]), FMagazine.Count * 4, PChar(Pointer(Result)), len);
end;

function TLR1.GetState: UCS4Char;
begin
  Result:= FMagazine[FMagazine.Count - 1];
end;

function TLR1.IsEmptyMagazine: Boolean;
begin
  Result:= FMagazine.Count = 0;
end;

function TLR1.IsMagazineEqual(Value: UCS4Char): Boolean;
begin
  if FMagazine.Count > 0 then
    Result:= FMagazine.Last = Value
  else
    Result:= False;
end;

function TLR1.IsTapeEnd: Boolean;
begin
  Result:= FTape.EoF;
end;

class procedure TLR1.PopAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PopState;
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

function TLR1.PopMagazineAsString(BeginPosition: Integer): string;
var len, count: Integer;
begin
  count:= FMagazine.Count - BeginPosition;
  len:= TPublicEncoding(TUCS4Encoding.UCS4).GetCharCount(PByte(@FMagazine.List[BeginPosition]), count * 4);
  SetLength(Result, len);
  TPublicEncoding(TUCS4Encoding.UCS4).GetChars(PByte(@FMagazine.List[BeginPosition]), count * 4, PChar(Pointer(Result)), len);
  FMagazine.DeleteRange(BeginPosition, count);
end;

function TLR1.PopState: UCS4Char;
begin
  Result:= FMagazine[FMagazine.Count - 1];
  FMagazine.Delete(FMagazine.Count - 1);
end;

class procedure TLR1.PushAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PushState(ACurrentKey);
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.PushAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer);
begin
  Self.PushState(ACurrentKey);
  Self.ToNextKey;
end;

procedure TLR1.PushKey(AKey: UCS4Char);
begin
  AddUCS4ToString(AKey, FCurrentString);
end;

procedure TLR1.PushState(State: UCS4Char);
begin
  FMagazine.Add(State);
end;

class procedure TLR1.ReadAndBack(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PushKey(ACurrentKey);
  Self.ToNextKey;
  Self.CurrentState:= Self.PopState - $20000;
end;

class procedure TLR1.ReadAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PushKey(ACurrentKey);
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.ReadAndGo(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.PushKey(ACurrentKey);
  Self.ToNextKey;
end;

class procedure TLR1.RepeatAndBack(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.CurrentState:= Self.PopState - $20000;
end;

class procedure TLR1.RepeatAtNewState(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
end;

procedure TLR1.RunParse(ATape: TTapeReader; BeginState: Integer);
var PrevMagazine, PrevKey: UCS4Char;
    PrevState: Integer;
    PrevByMagazine: Integer;
    PrevByKey: Integer;

  procedure DoAction(const ByKey: TKeyTransition; Key: UCS4Char);
  begin
    PrevState:= FCurrentState;
    FCurrentState:= ByKey.NextState;
    ByKey.Action(Self, Key, PrevState);
  end;

  procedure FindByKey(const ByMagazine: TMagazineTransition; Key: UCS4Char; ARepeat: Boolean);
  var j: Integer;
      ByKey: PKeyTransitions;
  begin
    if ARepeat and (Key = PrevKey) then begin
      ByKey:= ByMagazine.KeyTransitions;
      DoAction(ByKey[PrevByKey], Key)
    end else begin
      for j := 0 to ByMagazine.KeyTransitionsLength - 1 do begin
        ByKey:= ByMagazine.KeyTransitions;
        if (Key >= ByKey[j].BeginKey) and (Key <= ByKey[j].EndKey) then begin
          PrevByKey:= j;
          PrevKey:= Key;
          DoAction(ByKey[j], Key);
          Exit;
        end;
      end;
      PrevState:= FCurrentState;
      FCurrentState:= -1;
    end;
  end;

  procedure OneStep(Key: UCS4Char);
  var i, j: Integer;
      OldState: Integer;
      Magazine: UCS4Char;
      ByMagazine: PMagazineTransitions;
      ByKey: PKeyTransitions;
  begin
    if FMagazine.Count = 0 then
      Magazine:= _end
    else
      Magazine:= FMagazine.Last;
    if (FCurrentState = PrevState) and (Magazine = PrevMagazine) then begin
      ByMagazine:= FStates[PrevState].MagazineTransitions;
      FindByKey(ByMagazine[PrevByMagazine], Key, True);
    end else begin
      for i := 0 to FStates[FCurrentState].MagazineTransitionsLength - 1 do begin
        ByMagazine:= FStates[FCurrentState].MagazineTransitions;
        if (Magazine >= ByMagazine[i].BeginMagazine) and (Magazine <= ByMagazine[i].EndMagazine) then begin
          PrevMagazine:= Magazine;
          PrevByMagazine:= i;
          FindByKey(ByMagazine[i], Key, False);
          Exit;
        end;
      end;
      PrevState:= FCurrentState;
      FCurrentState:= -1;
    end;
  end;
begin
  FTape:= ATape;

  FCurrentState:= BeginState;
  FMagazine.Clear;
  PrevState:= -1;

  while (FCurrentState >= 0) and (not FTape.EoF) do
    OneStep(FTape.Peek);

  //for repeats at end of file
  while FCurrentState >= 0 do
    OneStep(0);
end;

class procedure TLR1.SkipAndBack(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ToNextKey;
  Self.CurrentState:= Self.PopState - $20000;
end;

class procedure TLR1.SkipAndContinue(Self: TLR1; ACurrentKey: UCS4Char;
  ACurrentState: Integer);
begin
  Self.ToNextKey;
  Self.CurrentState:= ACurrentState;
end;

class procedure TLR1.SkipAndGo(Self: TLR1; ACurrentKey: UCS4Char; ACurrentState: Integer);
begin
  Self.ToNextKey;
end;

procedure TLR1.ToNextKey;
begin
  FTape.Read;
end;

{ TStreamTape }

constructor TStreamTape.Create(AStream: TStream; AEncoding: TEncoding; AOwner: Boolean);
begin
  FStream:= AStream;
  if AEncoding = nil then
    FEncoding:= TEncoding.Default
  else
    FEncoding:= AEncoding;
  FOwner:= AOwner;
end;

destructor TStreamTape.Destroy;
begin
  if FOwner then
    FStream.Free;
  inherited;
end;

function TStreamTape.Peek: UCS4Char;
begin
  inherited;
  while not FEndOfStream and ((FCharPosition >= FCharCount) or
      ((FCharPosition = FCharCount - 1) and IsFirstSurrogateChar(FCharBuffer[FCharPosition]))) do begin
    UpdateBuffer;
  end;
  Result:= SurrogateToUCS4Char(@FCharBuffer[FCharPosition]);
end;

procedure TStreamTape.SetEncoding(Value: TEncoding);
var Count: Integer;
begin
  if Value = nil then
    Value:= TEncoding.Default;
  if FEncoding <> Value then begin
    if FCharCount > 0 then begin
      Count:= TPublicEncoding(FEncoding).GetByteCount(@FCharBuffer[0], FCharCount);
      Dec(FOffset, Count);
      Count:= TPublicEncoding(FEncoding).GetByteCount(@FCharBuffer[0], FCharPosition);
      Inc(FOffset, Count);
      FCharCount:= 0;
    end;
    FEncoding:= Value;
  end;
end;

function TStreamTape.Skip: Boolean;
begin
  if IsFirstSurrogateChar(FCharBuffer[FCharPosition]) then
    Inc(FCharPosition);
  Inc(FCharPosition);
  FEoF:= FEndOfStream and (FCharCount <= FCharPosition);
  Result:= EoF;
end;

procedure TStreamTape.UpdateBuffer;
var CharCount, BufCount, M, MaxCharCount: Integer;
begin
  if FCount <= FOffset then begin
    FOffset:= 0;
    FCount:= FStream.Read(FBuffer, SizeOf(FBuffer));
  end;

  MaxCharCount:= Length(FCharBuffer);
  //can be tru only if we have one broken char in end of buffer
  if (FCharCount > FCharPosition) and IsFirstSurrogateChar(FCharBuffer[FCharPosition]) then begin
    Dec(MaxCharCount);
    FCharCount:= 1;
  end else
    FCharCount:= 0;

  BufCount:= FCount - FOffset;

  CharCount:= TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset], BufCount);
  if CharCount = 0 then begin
    M:= TPublicEncoding(FEncoding).MaxCharSize;//GetMaxByteCount(1);
    if M > BufCount then begin
      Move(FBuffer[FOffset], FBuffer[0], BufCount);
      FOffset:= 0;
      FCount:= FStream.Read(FBuffer[BufCount], SizeOf(FBuffer) - BufCount) + BufCount;
      FEndOfStream:= FCount = BufCount;
      BufCount:= FCount;
      if M > BufCount then
        raise Exception.Create('Wrong buffer ending');
    end;
    while CharCount = 0 do begin
      if TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset + BufCount - TPublicEncoding(FEncoding).MaxCharSize], M) > 0 then begin
        Dec(BufCount, TPublicEncoding(FEncoding).MaxCharSize - M);
        CharCount:= TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset], BufCount);
      end;
      Dec(M);
      if M <= 0 then begin
        //try get one char
        for M:= 1 to TPublicEncoding(FEncoding).MaxCharSize do begin
          CharCount:= TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset], M);
          if CharCount > 0 then begin
            BufCount:= M;
            Break;
          end;
        end;
        if CharCount > 0 then
          Break;
        raise Exception.Create('Wrong string encoding');
      end;
    end;
  end;

  while CharCount > MaxCharCount do begin
    M:= TPublicEncoding(FEncoding).MaxCharSize;
    repeat
      if M < 0 then
        raise Exception.Create('Wrong string encoding');
      Dec(M);
      if TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset + BufCount - TPublicEncoding(FEncoding).MaxCharSize], M) > 0 then begin
        Dec(BufCount, TPublicEncoding(FEncoding).MaxCharSize - M);
        CharCount:= TPublicEncoding(FEncoding).GetCharCount(@FBuffer[FOffset], BufCount);
      end;
    until CharCount <> 0;
  end;

  FCharPosition:= 0;
  FCharCount:= TPublicEncoding(FEncoding).GetChars(@FBuffer[FOffset], BufCount, @FCharBuffer[Length(FCharBuffer) - MaxCharCount], CharCount) + FCharCount;
  Inc(FOffset, BufCount);
  FEndOfStream:= (FCount < SizeOf(FBuffer)) and (FCount <= FOffset);
end;

{ TTapeReader }

function TTapeReader.Peek: UCS4Char;
begin
  if FEoF then
    raise Exception.Create('End of tape');
  Result:= 0;
end;

function TTapeReader.Read: UCS4Char;
begin
  Result:= Peek;
  Skip;
end;

{ TStringTape }

constructor TStringTape.Create(const AString: string);
begin
  FString:= AString;
  FCurrentPosition:= 0;
  FEoF:= FCurrentPosition >= Length(FString);
end;

function TStringTape.Peek: UCS4Char;
begin
  inherited;
  Result:= SurrogateToUCS4Char(@PChar(Pointer(FString))[FCurrentPosition]);
end;

function TStringTape.Skip: Boolean;
begin
  if IsFirstSurrogateChar(FString[FCurrentPosition + 1]) then
    Inc(FCurrentPosition);
  Inc(FCurrentPosition);
  FEoF:= FCurrentPosition >= Length(FString);
  Result:= FEoF;
end;

end.
