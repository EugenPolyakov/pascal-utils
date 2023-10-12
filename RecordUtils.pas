unit RecordUtils;

interface

uses System.RTLConsts, System.SysUtils, System.Generics.Defaults, System.Types,
  SysTypes, System.Generics.Collections, System.Math;

type
  TDummyArray<T> = array [0..0] of T;

  TLocalListRecord<T> = record
  private
    FItems: ^TDummyArray<T>;
    FCapacity: Integer;
    FCount: Integer;
    function GetItem(Index: Integer): T; inline;
    procedure SetItem(Index: Integer; const Value: T); inline;
    procedure CheckIndex(Index: Integer); inline;
    procedure InitializeRaw(var AData; ACapacity: Integer; ACount: Integer); inline;
  public
    procedure Initialize(var AData: TDummyArray<T>; ACapacity: Integer; ACount: Integer = 0); overload;
    procedure Initialize(var AData: T; ACapacity: Integer; ACount: Integer = 0); overload; inline;
    property Capacity: Integer read FCapacity;
    property Count: Integer read FCount;
    function Add(const AValue: T): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TBuffer<T> = record
    Buffer: TArray<T>;
    procedure SetSize(ANewSize: Integer); overload; inline;
    procedure SetSize(ANewSize: Integer; AReset: Boolean); overload; inline;
    class procedure AddToRingBuffer(ABuffer: TArray<T>; var Offset: Integer; AValue: T); inline; static;
  end;

  TListRecord<T> = record
  public type
    TListRecordEnumerator = record
    private
      FOffset: Integer;
      FList: ^TListRecord<T>;
      function GetCurrent: T;
    public
      constructor Create(const AList: TListRecord<T>);
      function MoveNext: Boolean;
      property Current: T read GetCurrent;
    end;
    TListRecordWrapper = record
    private
      FList: ^TListRecord<T>;
    public
      constructor Create(const AList: TListRecord<T>);
      function GetEnumerator: TListRecordEnumerator;
    end;
  private
    FItems: TArray<T>;
    FCount: Integer;
    FComparer: IComparer<T>;
    //FOnNotify: TCollectionNotifyEvent<T>;
    //FArrayManager: TArrayManager<T>;
    function GetCapacity: Integer; inline;
    procedure SetCapacity(Value: Integer); inline;
    procedure SetCount(Value: Integer); inline;
    function GetItem(Index: Integer): T; inline;
    procedure SetItem(Index: Integer; const Value: T); inline;
    procedure Grow(ACount: Integer);
    procedure GrowCheck(ACount: Integer); inline;
  public
    constructor Create(const AComparer: IComparer<T>; ACapacity: Integer = 10); overload;
    constructor Create(ACapacity: Integer); overload;
    procedure Insert(Index: Integer; const Value: T);
    function Add(const Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    property Comparer: IComparer<T> read FComparer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property List: TArray<T> read FItems;
    function GetEnumerator: TListRecordEnumerator;
    function ToArray: TArray<T>;
    procedure TrimExcess; inline;
    function Last: T;
    procedure Clear;
    function IndexOf(const Value: T): Integer;
    function Remove(const Value: T): Integer;
    function Contains(const Value: T): Boolean;
  end;

  TSparseSection<T> = record
    FItems: TArray<T>;
    FOffset: Integer;
  end;
  //PSparseSection<T> = ^TSparseSection<T>;

  TFictiveSparseSection = record
    FItems: TArray<Integer>;
    FOffset: Integer;
  end;
  PFictiveSparseSection = ^TFictiveSparseSection;

  TSparseListRecord<T> = record
  const
    DefaultSparseSizeInBytes = 4080; //~4KB
    MaxSparseSizeInBytes = 4194200; //~4MB
  private
    FItems: TArray<TSparseSection<T>>;
    FCount: Integer;
    //FOnNotify: TCollectionNotifyEvent<T>;
    //FArrayManager: TArrayManager<T>;
    function GetItem(Index: Integer): T; //inline;
    procedure SetItem(Index: Integer; const Value: T); //inline;
  public
    function Add(const Value: T): Integer;
    //procedure Delete(Index: Integer);
    //procedure DeleteRange(AIndex, ACount: Integer);
    property Count: Integer read FCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    procedure Clear;
    //function ToArray: TArray<T>;
  end;

  TBitsSparseRecord = record
  private
    FBits: TSparseListRecord<LongWord>;
    procedure Error;
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    function GetEnumerator: IEnumerator<Integer>;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    //property Raw: TArray<LongWord> read FBits;
    //property Size: Integer read FSize write SetSize;
    function OpenBit: Integer;
    procedure Clear;
    function Any: Boolean;
    constructor Clone(const Buf: TBitsSparseRecord);
  end;

  TBitsSparseRecordEnumerator = class (TInterfacedObject, IEnumerator<Integer>)
  private
    FLastIndex: Integer;
    FBits: TBitsSparseRecord;
  protected
  public
    function MoveNext: Boolean;
    procedure Reset;
    function GetCurrentGeneric: Integer;
    function IEnumerator<Integer>.GetCurrent = GetCurrentGeneric;
    function GetCurrent: TObject;
    constructor Create(const ABits: TBitsSparseRecord);
    destructor Destroy; override;
  end;

  TBitsRecord = record
  private
    FBits: TArray<LongWord>;
    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    function GetEnumerator: IEnumerator<Integer>;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Raw: TArray<LongWord> read FBits;
    //property Size: Integer read FSize write SetSize;
    function OpenBit: Integer;
    procedure Clear;
    function Any: Boolean;
    procedure Initialize(const ABits: TArray<LongWord>; AClone: Boolean = False);
    constructor Clone(const Buf: TBitsRecord);
  end;

  TBitsRecordEnumerator = class (TInterfacedObject, IEnumerator<Integer>)
  private
    FLastIndex: Integer;
    FBits: TBitsRecord;
  protected
  public
    function MoveNext: Boolean;
    procedure Reset;
    function GetCurrentGeneric: Integer;
    function IEnumerator<Integer>.GetCurrent = GetCurrentGeneric;
    function GetCurrent: TObject;
    constructor Create(const ABits: TBitsRecord);
    destructor Destroy; override;
  end;

  TOneLinkList<T> = record
    Value: T;
    Next: ^TOneLinkList<T>;
  end;

  TLockFreeStack<T> = record
  private type
  private
    FLast: ^TOneLinkList<T>;
  public
    procedure Push(const AValue: T);
    function Pop: T;
    function IsEmpty: Boolean;
  end;

  //lock-free one reader many writers
  TLockFreeORMWQueue<T> = record
  private
    FTail: ^TOneLinkList<T>;
    FHead: ^TOneLinkList<T>;
    FDummy: TOneLinkList<T>;
  public
    procedure Add(const AValue: T);
    function Deque: T;
    function IsEmpty: Boolean;
    procedure Initialize;
  end;

  TRegion = record
  private
    FRectangles: TListRecord<TRect>;
    FBoundRect: TRect;
    ///
    ///  Удалить из Value R, получившиеся прямоугольники добавить в массив
    ///  True - ничего не осталось
    ///
    function ExcludeFromRect(var Value: TRect; const R: TRect): Boolean;
  public
    procedure Initialize;
    property BoundRect: TRect read FBoundRect;
    procedure Include(const R: TRect);
    procedure Exclude(const R: TRect);
    function IntersectsWith(const R: TRect): Boolean;
    function IsEmpty: Boolean; inline;
  end;

  TPrefixStringTree<T> = record
  private type
    TIndex = record
      Offset, Size: Integer;
      IsLeaf: Boolean;
      Index: Integer;
    end;
    TNode = record
      Key: string;
      Children: array of TIndex;
    end;
    TFindResult = record
      Block, Index, CommonLength, LeftLength: Integer;
    end;
  private
    FValues: TArray<T>;
    FKeys: TArray<TNode>;
    FLastOffset: Integer;
    FParsedString: string;
    function FindNode(P: PChar; Len: Integer; out FindResult: TFindResult): Boolean;
    procedure SplitAndAdd(const AValue: T; Block, Index, UnionLength: Integer; const Suffix: string);
    procedure SaveToNode(const AValue: T; Block: Integer; const Suffix: string);
  public
    procedure Reset(const AValue: string);
    function NextToken: T;
    function TryNextToken(var Token: T): Boolean;
    function IsEnd: Boolean;
    procedure AddOrSetValue(const Prefix: string; const AValue: T);
    procedure Clear;
  end;

function Compare_TFictiveSparseSection(Inst: Pointer; const Left, Right: TFictiveSparseSection): Integer;
function GetStringUnionLength(P1, P2: PChar; MaxLength: Integer): Integer;

const
  BitsPerInt = SizeOf(LongWord) * 8;
  Comparer_Vtable_TFictiveSparseSection: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_TFictiveSparseSection
  );
  TFictiveSparseSectionComparerData: Pointer = @Comparer_Vtable_TFictiveSparseSection;

implementation

var
  TFictiveSparseSectionComparer: IComparer<TFictiveSparseSection>;

function Compare_TFictiveSparseSection(Inst: Pointer; const Left, Right: TFictiveSparseSection): Integer;
begin
  { Use subtraction }
  Result := Left.FOffset - Right.FOffset;
end;

function GetStringUnionLength(P1, P2: PChar; MaxLength: Integer): Integer;
begin
  Result:= 0;
  while Result < MaxLength do begin
    if P1^ <> P2^ then
      Exit(Result);

    Inc(P1);
    Inc(P2);
    Inc(Result);
  end;
end;

{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}

{ TLocalListRecord<T> }

function TLocalListRecord<T>.Add(const AValue: T): Integer;
begin
  if FCount >= FCapacity then
    raise EOutOfMemory.Create('TLocalListRecord<T>.Add');
  FItems[FCount]:= AValue;
  Inc(FCount);
end;

procedure TLocalListRecord<T>.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

procedure TLocalListRecord<T>.Clear;
begin
  FCount:= 0;
end;

procedure TLocalListRecord<T>.Delete(Index: Integer);
begin
  CheckIndex(Index);
  Dec(FCount);
  FItems[Index]:= Default(T);
  if Index <> Count then begin
    System.Move(FItems[Index + 1], FItems[Index], (Count - Index) * SizeOf(T));
    FillChar(FItems[Count], SizeOf(T), 0);
  end;
end;

function TLocalListRecord<T>.GetItem(Index: Integer): T;
begin
  CheckIndex(Index);
  Result:= FItems[Index];
end;

procedure TLocalListRecord<T>.Initialize(var AData: T; ACapacity, ACount: Integer);
begin
  InitializeRaw(AData, ACapacity, ACount);
end;

procedure TLocalListRecord<T>.Initialize(var AData: TDummyArray<T>; ACapacity, ACount: Integer);
begin
  FItems:= @AData;
  FCapacity:= ACapacity;
  FCount:= ACount;
end;

procedure TLocalListRecord<T>.InitializeRaw(var AData; ACapacity: Integer; ACount: Integer);
begin
  Initialize(TDummyArray<T>(AData), ACapacity, ACount);
end;

{procedure TLocalListRecord<T>.Move(CurIndex, NewIndex: Integer);
var
  temp: T;
begin
  if CurIndex = NewIndex then
    Exit;
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  temp := FItems[CurIndex];
  FItems[CurIndex] := Default(T);
  if CurIndex < NewIndex then
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(T))
  else
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(T));

  System.FillChar(FItems[NewIndex], SizeOf(T), 0);
  FItems[NewIndex] := temp;
end;}

procedure TLocalListRecord<T>.SetItem(Index: Integer; const Value: T);
begin
  CheckIndex(Index);
  FItems[Index]:= Value;
end;

{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}

{ TListRecord<T> }

function TListRecord<T>.Add(const Value: T): Integer;
begin
  GrowCheck(Count + 1);
  Result := Count;
  FItems[Count] := Value;
  Inc(FCount);
  //Notify(Value, cnAdded);
end;

procedure TListRecord<T>.Clear;
begin
  FCount:= 0;
  FItems:= nil;
end;

function TListRecord<T>.Contains(const Value: T): Boolean;
begin
  Result:= IndexOf(Value) >= 0;
end;

constructor TListRecord<T>.Create(const AComparer: IComparer<T>; ACapacity: Integer);
begin
  FComparer := AComparer;
  if FComparer = nil then
    FComparer := TComparer<T>.Default;
  FCount:= 0;
  FItems:= nil;
  Capacity:= ACapacity;
end;

constructor TListRecord<T>.Create(ACapacity: Integer);
begin
  Create(TComparer<T>.Default, ACapacity);
end;

procedure TListRecord<T>.Delete(Index: Integer);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  oldItem := FItems[Index];
  FItems[Index] := Default(T);
  Dec(FCount);
  if Index <> Count then
  begin
    System.Move(FItems[Index + 1], FItems[Index], (Count - Index) * SizeOf(T));
    System.FillChar(FItems[Count], SizeOf(T), 0);
  end;
  //Notify(oldItem, Notification);
end;

procedure TListRecord<T>.DeleteRange(AIndex, ACount: Integer);
var
  //oldItems: array of T;
  tailCount, I: Integer;
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > Count)
    or (AIndex + ACount < 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit;

  for i := AIndex to ACount + AIndex - 1 do
    FItems[I]:= Default(T);

  tailCount := Count - (AIndex + ACount);
  if tailCount > 0 then
  begin
    System.Move(FItems[AIndex + ACount], FItems[AIndex], tailCount * SizeOf(T));
    System.FillChar(FItems[Count - ACount], ACount * SizeOf(T), 0);
  end else
    System.FillChar(FItems[AIndex], ACount * SizeOf(T), 0);

  Dec(FCount, ACount);

  //for I := 0 to Length(oldItems) - 1 do
  //  Notify(oldItems[I], cnRemoved);
end;

function TListRecord<T>.GetCapacity: Integer;
begin
  Result := Length(FItems);
end;

function TListRecord<T>.GetEnumerator: TListRecordEnumerator;
begin
  Result.Create(Self);
end;

function TListRecord<T>.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

procedure TListRecord<T>.Grow(ACount: Integer);
var
  newCount: Integer;
begin
  newCount := Length(FItems);
  if newCount = 0 then
    newCount := ACount
  else
    repeat
      newCount := newCount * 2;
      if newCount < 0 then
        OutOfMemoryError;
    until newCount >= ACount;
  Capacity := newCount;
end;

procedure TListRecord<T>.GrowCheck(ACount: Integer);
begin
  if ACount > Length(FItems) then
    Grow(ACount)
  else if ACount < 0 then
    OutOfMemoryError;
end;

function TListRecord<T>.IndexOf(const Value: T): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(Value, FItems[i]) = 0 then
      Exit(i);
  Result:= -1;
end;

procedure TListRecord<T>.Insert(Index: Integer; const Value: T);
begin
  if (Index < 0) or (Index > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  GrowCheck(Count + 1);
  if Index <> Count then
  begin
    System.Move(FItems[Index], FItems[Index + 1], (Count - Index) * SizeOf(T));
    System.FillChar(FItems[Index], SizeOf(T), 0);
  end;
  FItems[Index] := Value;
  Inc(FCount);
end;

function TListRecord<T>.Last: T;
begin
  Result:= GetItem(Count - 1);
end;

function TListRecord<T>.Remove(const Value: T): Integer;
begin
  Result:= IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

procedure TListRecord<T>.SetCapacity(Value: Integer);
begin
  if Value < Count then
    Count := Value;
  SetLength(FItems, Value);
end;

procedure TListRecord<T>.SetCount(Value: Integer);
begin
  if Value < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if Value > Capacity then
    SetCapacity(Value);
  if Value < Count then
    DeleteRange(Value, Count - Value);
  FCount := Value;
end;

procedure TListRecord<T>.SetItem(Index: Integer; const Value: T);
var
  oldItem: T;
begin
  if (Index < 0) or (Index >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  //oldItem := FItems[Index];
  FItems[Index] := Value;

  //Notify(oldItem, cnRemoved);
  //Notify(Value, cnAdded);
end;

function TListRecord<T>.ToArray: TArray<T>;
begin
  Result:= Copy(FItems, 0, Count);
end;

procedure TListRecord<T>.TrimExcess;
begin
  Capacity:= Count;
end;

{ TRegion }

procedure TRegion.Exclude(const R: TRect);
var i, oldCount: Integer;
    newBound: TRect;
begin
  if (FRectangles.Count > 0) and FBoundRect.IntersectsWith(R) then begin
    newBound.Top:= Integer.MaxValue;
    newBound.Left:= Integer.MaxValue;
    newBound.Right:= Integer.MinValue;
    newBound.Bottom:= Integer.MinValue;
    oldCount:= FRectangles.Count;
    for i := FRectangles.Count - 1 downto 0 do
      if ExcludeFromRect(FRectangles.List[i], R) then begin
        FRectangles.Delete(i);
        Dec(oldCount);
      end else begin
        if newBound.Left > FRectangles.List[i].Left then
          newBound.Left:= FRectangles.List[i].Left;
        if newBound.Top > FRectangles.List[i].Top then
          newBound.Top:= FRectangles.List[i].Top;
        if newBound.Right < FRectangles.List[i].Right then
          newBound.Right:= FRectangles.List[i].Right;
        if newBound.Bottom < FRectangles.List[i].Bottom then
          newBound.Bottom:= FRectangles.List[i].Bottom;
      end;
    for i := oldCount to FRectangles.Count - 1 do begin
      if newBound.Left > FRectangles.List[i].Left then
        newBound.Left:= FRectangles.List[i].Left;
      if newBound.Top > FRectangles.List[i].Top then
        newBound.Top:= FRectangles.List[i].Top;
      if newBound.Right < FRectangles.List[i].Right then
        newBound.Right:= FRectangles.List[i].Right;
      if newBound.Bottom < FRectangles.List[i].Bottom then
        newBound.Bottom:= FRectangles.List[i].Bottom;
    end;
    FBoundRect:= newBound;
  end;
end;

function TRegion.ExcludeFromRect(var Value: TRect; const R: TRect): Boolean;
var newRect: TRect;
begin
  Result:= False;
  if Value.Bottom < R.Top then
    ///
    ///   +---+
    ///   |   |
    ///   +---+
    ///  +-----+
    ///  |/////|
    ///  +-----+
    ///
  else if Value.Top > R.Bottom then
    ///
    ///  +-----+
    ///  |/////|
    ///  +-----+
    ///   +---+
    ///   |   |
    ///   +---+
    ///
  else if Value.Right < R.Left then
    ///
    ///         +---+
    ///  +----+ |///|
    ///  |    | |///|
    ///  |    | |///|
    ///  +----+ |///|
    ///         +---+
    ///
  else if Value.Left > R.Right then
    ///
    ///  +---+
    ///  |///| +----+
    ///  |///| |    |
    ///  |///| |    |
    ///  |///| +----+
    ///  +---+
    ///
  else if Value.Top < R.Top then begin
    if Value.Bottom <= R.Bottom then begin
      newRect.Top:= R.Top;
      newRect.Bottom:= Value.Bottom;
      Value.Bottom:= R.Top - 1;  //0
      if Value.Left < R.Left then begin
        newRect.Left:= Value.Left;
        newRect.Right:= R.Left - 1;
        FRectangles.Add(newRect); //1
        if Value.Right <= R.Right then begin
          ///
          ///  +-----+
          ///  |  0  |
          ///  +--+--+----+
          ///  |1 |//|////|
          ///  +--+--+////|
          ///     |///////|
          ///     +-------+
          ///
        end else begin
          ///
          ///  +------+
          ///  |  0   |
          ///  +-+--+-+
          ///  |1|//|2|
          ///  +-+--+-+
          ///    |//|
          ///    +--+
          ///
          newRect.Left:= R.Right + 1;
          newRect.Right:= Value.Right;
          FRectangles.Add(newRect);
        end;
      end else begin
        if Value.Right <= R.Right then begin
          ///
          ///    +------+
          ///    |  0   |
          ///  +-+------+-+
          ///  |/|//////|/|
          ///  |/+------+/|
          ///  |//////////|
          ///  +----------+
          ///
        end else begin
          ///
          ///    +-----+
          ///    |  0  |
          ///  +-+--+--|
          ///  |/|//| 1|
          ///  |/+--+--+
          ///  |////|
          ///  +----+
          newRect.Left:= R.Right + 1;
          newRect.Right:= Value.Right;
          FRectangles.Add(newRect);
        end;
      end;
    end else begin
      newRect.Top:= R.Bottom + 1;
      newRect.Bottom:= Value.Bottom;
      Value.Bottom:= R.Top - 1; //0
      if Value.Left < R.Left then begin
        newRect.Left:= Value.Left;
        newRect.Right:= Value.Right;
        FRectangles.Add(newRect); //1
        newRect.Top:= R.Top;
        newRect.Bottom:= R.Bottom;
        newRect.Right:= R.Left - 1;
        FRectangles.Add(newRect); //2
        if Value.Right <= R.Right then begin
          ///
          ///  +-----+
          ///  |  0  |
          ///  +--+--+----+
          ///  |2 |//|////|
          ///  +--+--+----+
          ///  |  1  |
          ///  +-----+
          ///
        end else begin
          ///
          ///  +--------+
          ///  |    0   |
          ///  +--+--+--+
          ///  |2 |//| 3|
          ///  +--+--+--+
          ///  |   1    |
          ///  +--------+
          ///
          newRect.Left:= R.Right + 1;
          newRect.Right:= Value.Right;
          FRectangles.Add(newRect);
        end;
      end else begin
        newRect.Left:= Value.Left;
        newRect.Right:= Value.Right;
        FRectangles.Add(newRect);
        if Value.Right <= R.Right then begin
          ///
          ///     +-------+
          ///     |  0    |
          ///  +--+-------+--+
          ///  |//|///////|//|
          ///  +--+-------+--+
          ///     |  1    |
          ///     +-------+
          ///
        end else begin
          ///
          ///     +-----+
          ///     |  0  |
          ///  +--+--+--+
          ///  |//|//| 2|
          ///  +--+--+--+
          ///     |  1  |
          ///     +-----+
          ///
          newRect.Top:= R.Top;
          newRect.Bottom:= R.Bottom;
          newRect.Left:= R.Right + 1;
          FRectangles.Add(newRect);
        end;
      end;
    end;
  end else begin
    if Value.Bottom <= R.Bottom then begin
      if Value.Left < R.Left then begin
        if Value.Right <= R.Right then begin
          ///
          ///     +------+
          ///     |//////|
          ///  +--+---+//|
          ///  |0 |///|//|
          ///  +--+---+//|
          ///     |//////|
          ///     +------+
          ///
          Value.Right:= R.Left - 1;
        end else begin
          ///
          ///     +--+
          ///     |//|
          ///  +--+--+--+
          ///  |0 |//| 1|
          ///  +--+--+--+
          ///     |//|
          ///     +--+
          ///
          newRect:= Value;
          newRect.Left:= R.Right + 1;
          Value.Right:= R.Left - 1;
          FRectangles.Add(newRect);
        end;
      end else begin
        if Value.Right <= R.Right then begin
          ///
          ///  +---------+
          ///  |/////////|
          ///  |//+---+//|
          ///  |//|/0/|//|
          ///  |//+---+//|
          ///  |/////////|
          ///  +---------+
          ///
          Result:= True;
        end else begin
          ///
          ///  +-----+
          ///  |/////|
          ///  |//+--+--+
          ///  |//|//| 0|
          ///  |//+--+--+
          ///  |/////|
          ///  +-----+
          ///
          Value.Right:= R.Left - 1;
        end;
      end;
    end else begin
      if Value.Left < R.Left then begin
        newRect.Bottom:= Value.Bottom;
        newRect.Right:= Value.Right;
        Value.Right:= R.Left - 1; //0
        if Value.Right <= R.Right then begin
          ///
          ///       +-----+
          ///       |/////|
          ///  +----+--+//|
          ///  |    |//|//|
          ///  | 0  +--+--+
          ///  |    | 1|
          ///  +----+--+
          ///
          newRect.Top:= R.Bottom + 1;
          newRect.Left:= R.Left;
          FRectangles.Add(newRect);
        end else begin
          ///
          ///      +----+
          ///      |////|
          ///  +---+----+---+
          ///  |   |////|   |
          ///  |0  +----+  1|
          ///  |   |  2 |   |
          ///  +---+----+---+
          ///
          newRect.Top:= Value.Top;
          newRect.Left:= R.Right + 1;
          FRectangles.Add(newRect);
          newRect.Left:= R.Left;
          newRect.Right:= R.Right;
          newRect.Top:= R.Bottom + 1;
          FRectangles.Add(newRect);
        end;
      end else begin
        if Value.Right <= R.Right then begin
          ///
          ///  +---------+
          ///  |/////////|
          ///  |//+---+//|
          ///  |//|///|//|
          ///  +--+---+--+
          ///     | 0 |
          ///     +---+
          ///
          Value.Top:= R.Bottom + 1;
        end else begin
          ///
          ///  +----+
          ///  |////|
          ///  |//+-+---+
          ///  |//|/|   |
          ///  +--+-+ 0 |
          ///     |1|   |
          ///     +-+---+
          newRect.Left:= Value.Left;
          Value.Left:= R.Right + 1;
          newRect.Top:= R.Bottom + 1;
          newRect.Right:= R.Right;
          newRect.Bottom:= Value.Bottom;
          FRectangles.Add(newRect);
        end;
      end;
    end;
  end;
end;

procedure TRegion.Include(const R: TRect);
var i: Integer;
    tmpRegion: TRegion;
begin
  if FRectangles.Count = 0 then begin
    FRectangles.Add(R);
    FBoundRect:= R;
  end else if R.Contains(FBoundRect) then begin
    FRectangles.Count:= 1;
    FRectangles[0]:= R;
    FBoundRect:= R;
  end else begin
    tmpRegion.Initialize;
    for i := FRectangles.Count - 1 downto 0 do
      tmpRegion.Exclude(FRectangles.List[i]);
    if tmpRegion.FRectangles.Count > 0 then begin
      for i := tmpRegion.FRectangles.Count - 1 to 0 do
        FRectangles.Add(tmpRegion.FRectangles.List[i]);
      if FBoundRect.Left > tmpRegion.FBoundRect.Left then
        FBoundRect.Left:= tmpRegion.FBoundRect.Left;
      if FBoundRect.Top > tmpRegion.FBoundRect.Top then
        FBoundRect.Top:= tmpRegion.FBoundRect.Top;
      if FBoundRect.Right < tmpRegion.FBoundRect.Right then
        FBoundRect.Right:= tmpRegion.FBoundRect.Right;
      if FBoundRect.Bottom < tmpRegion.FBoundRect.Bottom then
        FBoundRect.Bottom:= tmpRegion.FBoundRect.Bottom;
    end;
  end;
end;

procedure TRegion.Initialize;
begin
  FillChar(Self, SizeOf(Self), 0);
  FRectangles.Create(nil);
end;

function TRegion.IntersectsWith(const R: TRect): Boolean;
var i: Integer;
begin
  if (FRectangles.Count > 0) and FBoundRect.IntersectsWith(R) then begin
    for i := 0 to FRectangles.Count - 1 do
      if FRectangles.List[i].IntersectsWith(R) then
        Exit(True);
  end;
  Result:= False;
end;

function TRegion.IsEmpty: Boolean;
begin
  Result:= FRectangles.Count = 0;
end;

{ TLockFreeStack<T> }

function TLockFreeStack<T>.IsEmpty: Boolean;
begin
  Result:= FLast = nil;
end;

function TLockFreeStack<T>.Pop: T;
var elem: ^TOneLinkList<T>;
begin
  repeat
    elem:= FLast;
    if elem = nil then
      Exit(Default(T));
  until AtomicCmpExchange(FLast, elem.Next, elem) = elem;
  Result:= elem.Value;
  Dispose(elem);
end;

procedure TLockFreeStack<T>.Push(const AValue: T);
var elem: ^TOneLinkList<T>;
begin
  New(elem);
  elem.Value:= AValue;

  repeat
    elem.Next:= FLast;
  until AtomicCmpExchange(FLast, elem, elem.Next) = elem.Next;
end;

{ TBitsRecord }

function TBitsRecord.Any: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FBits) do
    if FBits[I] <> 0 then
      Exit(True);
  Result:= False;
end;

procedure TBitsRecord.Clear;
begin
  FBits:= nil;
end;

constructor TBitsRecord.Clone(const Buf: TBitsRecord);
begin
  Initialize(Buf.FBits, True);
end;

procedure TBitsRecord.Error;
begin
  raise EArgumentOutOfRangeException.CreateRes(@SBitsIndexError);
end;

function TBitsRecord.GetBit(Index: Integer): Boolean;
{$IFNDEF CPUX86}
var
  LMask: Integer;
begin
  if Index >= Length(FBits) * BitsPerInt then
    Exit(False);
  if Index < 0 then
    Error;

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));
  Result := (FBits[Index div BitsPerInt] and LMask) <> 0;
end;
{$ELSE CPUX86}
asm
        CMP     Index, 0
        JL      TBitsRecord.Error
        MOV     ECX, [EAX].FBits
        TEST    ECX, ECX
        JZ      @@returnFalse
        MOV     ECX, [ECX - 4]
        SHL     ECX, 5
        CMP     Index, ECX
        JGE     @@returnFalse
        MOV     EAX, [EAX].FBits
        BT      [EAX], Index
        SBB     EAX, EAX
        AND     EAX, 1
        RET
@@returnFalse:
        XOR EAX, EAX
end;
{$ENDIF CPUX86}

function TBitsRecord.GetEnumerator: IEnumerator<Integer>;
begin
  Result:= TBitsRecordEnumerator.Create(Self);
end;

procedure TBitsRecord.Initialize(const ABits: TArray<LongWord>; AClone: Boolean);
begin
  if AClone then begin
    SetLength(FBits, Length(ABits));
    Move(ABits[0], FBits[0], Length(ABits) * SizeOf(LongWord));
  end else
    FBits:= ABits;
end;

function TBitsRecord.OpenBit: Integer;
var
  I: Integer;
  E: LongWord;
begin
  for I := 0 to High(FBits) do
    if FBits[I] <> $FFFFFFFF then begin
      E:= 1;
      repeat
        if FBits[I] and E = 0 then begin
          Result:= GetIndexOfSetBit(E) + I * BitsPerInt;
          Exit;
        end;
        E:= E shr 1;
      until (E = 0);
    end;
  Result:= High(FBits) * BitsPerInt + 1;
end;

procedure TBitsRecord.SetBit(Index: Integer; Value: Boolean);
{$IFNDEF CPUX86}
var
  LRelInt: PLongWord;
  LMask: Integer;
begin
  if Index < 0 then
    Error;

  if Index >= Length(FBits) * BitsPerInt then
    SetSize(Index + 1);

  { Calculate the address of the related integer }
  LRelInt := @FBits[Index div BitsPerInt];

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));

  { Update the integer }
  if Value then
    LRelInt^ := LRelInt^ or LMask
  else
    LRelInt^ := LRelInt^ and not LMask;
end;
{$ELSE CPUX86}
asm
        CMP     Index,0
        JL      TBitsRecord.Error
        PUSH    EAX
        MOV     EAX, [EAX].FBits
        TEST    EAX, EAX
        JZ      @@Size
        MOV     EAX, [EAX - 4]
        SHL     EAX, 5
        CMP     Index, EAX
        JGE     @@Size
        POP     EAX

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        MOV     ECX, EDX
        SHR     ECX, 5
        LEA     EAX, [EAX + ECX * 4]
        AND     Index, $1F
        BTS     [EAX],Index
        RET

@@2:
        MOV     ECX, EDX
        SHR     ECX, 5
        LEA     EAX, [EAX + ECX * 4]
        AND     Index, $1F
        BTR     [EAX],Index
        RET

@@Size:
        MOV     EAX, [ESP]
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TBitsRecord.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;
{$ENDIF CPUX86}

procedure TBitsRecord.SetSize(Value: Integer);
var
  NewMemSize: Integer;
begin
  if Value <> Length(FBits) * BitsPerInt then
  begin
    if Value < 0 then Error;

    NewMemSize:= (Value + BitsPerInt - 1) div BitsPerInt;
    SetLength(FBits, NewMemSize);
    //FSize := NewMemSize * BitsPerInt;
  end;
end;

{ TBitsRecordEnumerator }

constructor TBitsRecordEnumerator.Create(const ABits: TBitsRecord);
begin
  FBits:= ABits;
  FLastIndex:= -1;
end;

destructor TBitsRecordEnumerator.Destroy;
begin

  inherited;
end;

function TBitsRecordEnumerator.GetCurrent: TObject;
begin
  raise ENotSupportedException.Create('GetCurrent: TObject');
end;

function TBitsRecordEnumerator.GetCurrentGeneric: Integer;
begin
  Result:= FLastIndex;
end;

function TBitsRecordEnumerator.MoveNext: Boolean;
var i, p: Integer;
    m: LongWord;
begin
  p:= (FLastIndex + 1) mod BitsPerInt;
  for I := (FLastIndex + 1) div BitsPerInt to High(FBits.FBits) do begin
    if FBits.FBits[I] <> 0 then begin
      m:= FBits.FBits[I] shr p;
      while m <> 0 do begin
        if m and 1 <> 0 then begin
          FLastIndex:= I * BitsPerInt + p;
          Exit(True);
        end;
        Inc(p);
        m:= m shr 1;
      end;
    end;
    p:= 0;
  end;
  FLastIndex:= Length(FBits.FBits) * BitsPerInt + 1;
  Result:= False;
end;

procedure TBitsRecordEnumerator.Reset;
begin
  FLastIndex:= -1;
end;

{ TBuffer<T> }

class procedure TBuffer<T>.AddToRingBuffer(ABuffer: TArray<T>;
  var Offset: Integer; AValue: T);
begin
  ABuffer[Offset]:= AValue;
  Offset:= (Offset + 1) mod Length(ABuffer);
end;

procedure TBuffer<T>.SetSize(ANewSize: Integer);
begin

end;

procedure TBuffer<T>.SetSize(ANewSize: Integer; AReset: Boolean);
begin

end;

{ TSparseListRecord<T> }

function TSparseListRecord<T>.Add(const Value: T): Integer;
var fi: TSparseSection<T>;
    rIndex, ofs: Integer;
begin
  if FItems = nil then begin
    SetLength(FItems, 1);
    FItems[0].FOffset:= 0;
    FCount:= 0;
  end;
  with FItems[High(FItems)] do begin
    if Length(FItems) >= DefaultSparseSizeInBytes div SizeOf(T) then begin
      SetLength(Self.FItems, Length(Self.FItems) + 1);
      Self.FItems[High(Self.FItems)].FOffset:= FOffset + Length(FItems);
    end;
  end;
  with FItems[High(FItems)] do begin
    SetLength(FItems, Length(FItems) + 1);
    FItems[High(FItems)]:= Value;
  end;
  Inc(FCount);
  Result:= FCount;
end;

procedure TSparseListRecord<T>.Clear;
begin
  FCount:= 0;
  FItems:= nil;
end;

function TSparseListRecord<T>.GetItem(Index: Integer): T;
var fi: TSparseSection<T>;
    rIndex, ofs: Integer;
begin
  if FItems <> nil then begin
    fi.FOffset:= Index;
    if not TArray.BinarySearch<TSparseSection<T>>(FItems, fi, rIndex,
        IComparer<TSparseSection<T>>(@TFictiveSparseSectionComparerData), 0, Length(FItems)) then
      Dec(rIndex);
    ofs:= Index - FItems[rIndex].FOffset;
    if ofs < Length(FItems[rIndex].FItems) then
      Result:= FItems[rIndex].FItems[ofs]
    else
      Result:= Default(T);
  end else
    Result:= Default(T);
end;

procedure TSparseListRecord<T>.SetItem(Index: Integer; const Value: T);
var fi: TSparseSection<T>;
    rIndex, ofs, i, nSize: Integer;
begin
  if FItems = nil then begin
    rIndex:= Index div (DefaultSparseSizeInBytes div SizeOf(T));
    SetLength(FItems, rIndex + 1);
    for i := 0 to rIndex do
      FItems[i].FOffset:= DefaultSparseSizeInBytes div SizeOf(T) * i;
    FCount:= 0;
  end;

  fi.FOffset:= Index;
  if not TArray.BinarySearch<TSparseSection<T>>(FItems, fi, rIndex,
      IComparer<TSparseSection<T>>(@TFictiveSparseSectionComparerData), 0, Length(FItems)) then
    Dec(rIndex);

  ofs:= Index - FItems[rIndex].FOffset;
  if ofs >= DefaultSparseSizeInBytes div SizeOf(T) then begin
    if rIndex = High(FItems) then begin
      Inc(rIndex);
      SetLength(FItems, rIndex + 1);
      FItems[rIndex].FOffset:= Index;
      ofs:= 0;
    end else if ofs >= MaxSparseSizeInBytes div SizeOf(T) then begin
      nSize:= ofs div (MaxSparseSizeInBytes div SizeOf(T));
      SetLength(FItems, Length(FItems) + nSize);
      Move(FItems[rIndex + 1], FItems[rIndex + nSize + 1], (Length(FItems) - rIndex - nSize - 1) * SizeOf(TSparseSection<T>));
      FillChar(FItems[rIndex + 1], nSize * SizeOf(TSparseSection<T>), 0);
      //CopyArray(Pointer(@FItems[rIndex + nSize + 1]), Pointer(@FItems[rIndex + 1]), TypeInfo(TSparseSection<T>), Length(FItems) - rIndex - nSize - 1);
      //fill blocks with max size
      for i := 1 to nSize - 1 do begin
        FItems[rIndex + i].FOffset:= FItems[rIndex].FOffset + i * (MaxSparseSizeInBytes div SizeOf(T));
        //FItems[rIndex + i].FItems:= nil;
      end;
      //if last block very small then do last 2 blocks same size
      if FItems[rIndex + nSize + 1].FOffset - Index < DefaultSparseSizeInBytes div SizeOf(T) then
        FItems[rIndex + nSize].FOffset:= FItems[rIndex + nSize - 1].FOffset + (Index - FItems[rIndex + nSize - 1].FOffset) div 2
      else
        FItems[rIndex + nSize].FOffset:= FItems[rIndex].FOffset + nSize * (MaxSparseSizeInBytes div SizeOf(T));
      //FItems[rIndex + nSize].FItems:= nil;
      Inc(rIndex, nSize);
      ofs:= Index - FItems[rIndex].FOffset;
    end;
  end;

  with FItems[rIndex] do begin
    if ofs >= Length(FItems) then
      SetLength(FItems, ofs + 1);
    FItems[ofs]:= Value;
  end;

  if Index >= FCount then
    FCount:= Index + 1;
end;

{ TBitsSparseRecord }

function TBitsSparseRecord.Any: Boolean;
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to High(FBits.FItems) do
    for J := 0 to High(FBits.FItems[I].FItems) do
      if FBits.FItems[I].FItems[J] <> 0 then
        Exit(True);
  Result:= False;
end;

procedure TBitsSparseRecord.Clear;
begin
  FBits.Clear;
end;

constructor TBitsSparseRecord.Clone(const Buf: TBitsSparseRecord);
var
  I, J, k: Integer;
  W: LongWord;
begin
  for I := 0 to High(FBits.FItems) do
    for J := 0 to High(FBits.FItems[I].FItems) do
      if FBits.FItems[I].FItems[J] <> 0 then begin
        W:= FBits.FItems[I].FItems[J];
        k:= 0;
        while W <> 0 do begin
          if W and 1 = 1 then
            SetBit(FBits.FItems[I].FOffset + J * BitsPerInt + k, True);
          W:= W shr 1;
          Inc(k);
        end;
      end;
end;

procedure TBitsSparseRecord.Error;
begin
  raise EArgumentOutOfRangeException.CreateRes(@SBitsIndexError);
end;

function TBitsSparseRecord.GetBit(Index: Integer): Boolean;
//{$IFNDEF CPUX86}
var
  LMask: Integer;
begin
  if Index >= FBits.Count * BitsPerInt then
    Exit(False);
  if Index < 0 then
    Error;

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));
  Result := (FBits[Index div BitsPerInt] and LMask) <> 0;
end;
(*{$ELSE CPUX86}
asm
        CMP     Index, 0
        JL      TBitsSparseRecord.Error
        MOV     ECX, [EAX].FBits
        TEST    ECX, ECX
        JZ      @@returnFalse
        MOV     ECX, [ECX - 4]
        SHL     ECX, 5
        CMP     Index, ECX
        JGE     @@returnFalse
        MOV     EAX, [EAX].FBits
        BT      [EAX], Index
        SBB     EAX, EAX
        AND     EAX, 1
        RET
@@returnFalse:
        XOR EAX, EAX
end;
{$ENDIF CPUX86} *)

function TBitsSparseRecord.GetEnumerator: IEnumerator<Integer>;
begin

end;

function TBitsSparseRecord.OpenBit: Integer;
var
  I: Integer;
  E, V: LongWord;
  J: Integer;
begin
  for I := 0 to High(FBits.FItems) do
    for J := 0 to High(FBits.FItems[I].FItems) do begin
      V:= FBits.FItems[I].FItems[J];
      if V <> $FFFFFFFF then begin
        E:= 1;
        repeat
          if V and E = 0 then begin
            Result:= GetIndexOfSetBit(E) + J * BitsPerInt + FBits.FItems[I].FOffset;
            Exit;
          end;
          E:= E shr 1;
        until (E = 0);
      end;
    end;
  Result:= (FBits.Count - 1) * BitsPerInt + 1;
end;

procedure TBitsSparseRecord.SetBit(Index: Integer; Value: Boolean);
var
  //LRelInt: PLongWord;
  LMask: LongWord;
begin
  if Index < 0 then
    Error;

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));

  { Update the integer }
  if Value then
    FBits[Index div BitsPerInt] := FBits[Index div BitsPerInt] or LMask
  else
    FBits[Index div BitsPerInt] := FBits[Index div BitsPerInt] and not LMask;
end;

{ TBitsSparseRecordEnumerator }

constructor TBitsSparseRecordEnumerator.Create(const ABits: TBitsSparseRecord);
begin
  FBits:= ABits;
  FLastIndex:= -1;
end;

destructor TBitsSparseRecordEnumerator.Destroy;
begin

  inherited;
end;

function TBitsSparseRecordEnumerator.GetCurrent: TObject;
begin
  raise ENotSupportedException.Create('GetCurrent: TObject');
end;

function TBitsSparseRecordEnumerator.GetCurrentGeneric: Integer;
begin
  Result:= FLastIndex;
end;

function TBitsSparseRecordEnumerator.MoveNext: Boolean;
var i, p: Integer;
    m: LongWord;
    fi: TSparseSection<LongWord>;
    rIndex{, ofs}: Integer;
  J: Integer;
begin
  if FBits.FBits.FItems <> nil then begin
    p:= (FLastIndex + 1) mod BitsPerInt;
    fi.FOffset:= (FLastIndex + 1) div BitsPerInt;
    if not TArray.BinarySearch<TSparseSection<LongWord>>(FBits.FBits.FItems, fi, rIndex,
        IComparer<TSparseSection<LongWord>>(@TFictiveSparseSectionComparerData), 0, Length(FBits.FBits.FItems)) then
      Dec(rIndex);
    //ofs:= fi.FOffset - FBits.FBits.FItems[rIndex].FOffset;
    for I := fi.FOffset - FBits.FBits.FItems[rIndex].FOffset to High(FBits.FBits.FItems[rIndex].FItems) do begin
      if FBits.FBits.FItems[rIndex].FItems[I] <> 0 then begin
        m:= FBits.FBits.FItems[rIndex].FItems[I] shr p;
        while m <> 0 do begin
          if m and 1 <> 0 then begin
            FLastIndex:= (FBits.FBits.FItems[rIndex].FOffset + I) * BitsPerInt + p;
            Exit(True);
          end;
          Inc(p);
          m:= m shr 1;
        end;
      end;
      p:= 0;
    end;
    for J := rIndex to High(FBits.FBits.FItems) do
      for I := 0 to High(FBits.FBits.FItems[J].FItems) do begin
        if FBits.FBits.FItems[J].FItems[I] <> 0 then begin
          p:= 0;
          m:= FBits.FBits.FItems[J].FItems[I] shr p;
          while m <> 0 do begin
            if m and 1 <> 0 then begin
              FLastIndex:= (FBits.FBits.FItems[J].FOffset + I) * BitsPerInt + p;
              Exit(True);
            end;
            Inc(p);
            m:= m shr 1;
          end;
        end;
      end;
  end;
  FLastIndex:= FBits.FBits.Count * BitsPerInt + 1;
  Result:= False;
end;

procedure TBitsSparseRecordEnumerator.Reset;
begin
  FLastIndex:= -1;
end;

{ TLockFreeORMWQueue<T> }

procedure TLockFreeORMWQueue<T>.Add(const AValue: T);
var elem, old: ^TOneLinkList<T>;
    succeeded: Boolean;
begin
  New(elem);
  elem.Value:= AValue;
  elem.Next:= nil;

  repeat
    old:= FTail;
    while old.Next <> nil do
      old:= old.Next;
  until AtomicCmpExchange(old.Next, elem, nil) = nil;

  AtomicCmpExchange(FTail, elem, old);
end;

function TLockFreeORMWQueue<T>.Deque: T;
var elem: ^TOneLinkList<T>;
begin
  repeat
    elem:= FHead;
    if elem.Next = nil then
      raise EListError.CreateRes(@SUnbalancedOperation);
  until AtomicCmpExchange(FHead, elem.Next, elem) = elem;

  Result:= elem.Next.Value;
  Dispose(elem);
end;

procedure TLockFreeORMWQueue<T>.Initialize;
begin
  FDummy.Next:= nil;
  FTail:= @FDummy;
  FHead:= @FDummy;
end;

function TLockFreeORMWQueue<T>.IsEmpty: Boolean;
var elem, next: ^TOneLinkList<T>;
begin
  elem:= FHead;
  Result:= elem.Next = nil;
end;

{ TPrefixStringTree<T> }

procedure TPrefixStringTree<T>.AddOrSetValue(const Prefix: string;
  const AValue: T);
var r: TFindResult;
begin
  if FKeys = nil then begin
    SetLength(FKeys, 1);
    FKeys[0].Key:= Prefix;
    SetLength(FKeys[0].Children, 1);
    with FKeys[0].Children[0] do begin
      Offset:= 1;
      Size:= Length(Prefix);
      IsLeaf:= True;
      Index:= 0;
    end;

    SetLength(FValues, 1);
    FValues[0]:= AValue;
  end else begin
    if FindNode(PChar(Prefix), Length(Prefix), r) then
      if r.LeftLength > 0 then
        raise EInvalidArgument.CreateFmt('Can''t create suffix: ''%s'', already has ''%s''',
            [Prefix, Copy(Prefix, 1, Length(Prefix) - r.LeftLength) +
            Copy(FKeys[r.Block].Key, FKeys[r.Block].Children[r.Index].Offset + r.CommonLength, FKeys[r.Block].Children[r.Index].Size - r.CommonLength)])
      else
        FValues[FKeys[r.Block].Children[r.Index].Index]:= AValue
    else if r.CommonLength = 0 then begin
        {with FKeys[r.Block].Children[r.Index] do
        if IsLeaf and (Size = ) then
          raise EInvalidArgument.CreateFmt('Cant create suffix: ''%s'', already has ''%s''',
              [Prefix, Copy(Prefix, 1, Length(Prefix) - r.LeftLength) +
              Copy(FKeys[r.Block].Key, FKeys[r.Block].Children[r.Index].Offset + r.CommonLength, FKeys[r.Block].Children[r.Index].Size - r.CommonLength)]);
        }
      if r.Index <> -1 then
        r.Block:= FKeys[r.Block].Children[r.Index].Index;
      with FKeys[r.Block] do
        SetLength(Children, Length(Children) + 1);
      SaveToNode(AValue, r.Block, Copy(Prefix, Length(Prefix) - r.LeftLength + 1));
    end else
      SplitAndAdd(AValue, r.Block, r.Index, r.CommonLength, Copy(Prefix, Length(Prefix) - r.LeftLength + 1));
  end;
end;

procedure TPrefixStringTree<T>.Clear;
begin
  FValues:= nil;
  FKeys:= nil;
end;

function TPrefixStringTree<T>.FindNode(P: PChar; Len: Integer; out FindResult: TFindResult): Boolean;
var
  i, node, lastNode: Integer;
  ofs: Integer;
begin
  node:= 0;
  repeat
    lastNode:= node;
    for i := 0 to High(FKeys[node].Children) do begin
      ofs:= GetStringUnionLength(@FKeys[node].Key[FKeys[node].Children[i].Offset],
          P, Min(FKeys[node].Children[i].Size, Len));
      if ofs = FKeys[node].Children[i].Size then begin
        Dec(Len, FKeys[node].Children[i].Size);
        if FKeys[node].Children[i].IsLeaf then begin
          FindResult.Block:= node;
          FindResult.Index:= i;
          FindResult.CommonLength:= ofs;
          FindResult.LeftLength:= Len;
          Exit(True);
        end else begin
          node:= FKeys[node].Children[i].Index;
          Inc(P, ofs);
          Break;
        end;
      end else if ofs > 0 then begin
        FindResult.Block:= node;
        FindResult.Index:= i;
        FindResult.CommonLength:= ofs;
        FindResult.LeftLength:= Len - ofs;
        Exit(False);
      end;
    end;
  until lastNode = node;

  FindResult.Block:= node;
  FindResult.Index:= -1;
  FindResult.CommonLength:= 0;
  FindResult.LeftLength:= Len;
  Exit(False);
end;

function TPrefixStringTree<T>.IsEnd: Boolean;
begin
  Result:= FLastOffset > Length(FParsedString);
end;

function TPrefixStringTree<T>.NextToken: T;
begin
  if not TryNextToken(Result) then
    raise EInvalidOpException.Create('No tokens');
end;

procedure TPrefixStringTree<T>.Reset(const AValue: string);
begin
  FParsedString:= AValue;
  FLastOffset:= 1;
end;

procedure TPrefixStringTree<T>.SaveToNode(const AValue: T; Block: Integer; const Suffix: string);
begin
  with FKeys[Block] do begin
    with Children[High(Children)] do begin
      Offset:= Length(Key) + 1;
      Size:= Length(Suffix);
      IsLeaf:= True;
      Index:= Length(FValues);
    end;
    Key:= Key + Suffix;
  end;

  SetLength(FValues, Length(FValues) + 1);
  FValues[High(FValues)]:= AValue;
end;

procedure TPrefixStringTree<T>.SplitAndAdd(const AValue: T; Block, Index,
  UnionLength: Integer; const Suffix: string);
begin
  SetLength(FKeys, Length(FKeys) + 1);
  with FKeys[High(FKeys)] do begin
    SetLength(Children, 2);
    Children[0].Offset:= 1;
    Children[0].Size:= FKeys[Block].Children[Index].Size - UnionLength;
    Children[0].IsLeaf:= FKeys[Block].Children[Index].IsLeaf;
    Children[0].Index:= FKeys[Block].Children[Index].Index;

    Key:= Copy(FKeys[Block].Key, FKeys[Block].Children[Index].Offset + UnionLength, FKeys[Block].Children[Index].Size - UnionLength);
  end;

  SaveToNode(AValue, High(FKeys), Suffix);

  with FKeys[Block].Children[Index] do begin
    Size:= UnionLength;
    Index:= High(FKeys);
    IsLeaf:= False;
  end;
end;

function TPrefixStringTree<T>.TryNextToken(var Token: T): Boolean;
var
  fr: TFindResult;
begin
  if FLastOffset > Length(FParsedString) then
    Exit(False);

  Result:= FindNode(@FParsedString[FLastOffset], Length(FParsedString) - FLastOffset + 1, fr);

  if Result then begin
    Token:= FValues[FKeys[fr.Block].Children[fr.Index].Index];
    FLastOffset:= Length(FParsedString) - fr.LeftLength + 1;
  end;
end;

{ TListRecord<T>.TListRecordEnumerator }

constructor TListRecord<T>.TListRecordEnumerator.Create(const AList: TListRecord<T>);
begin
  FOffset:= -1;
  FList:= @AList;
end;

function TListRecord<T>.TListRecordEnumerator.GetCurrent: T;
begin
  Result:= FList.Items[FOffset];
end;

function TListRecord<T>.TListRecordEnumerator.MoveNext: Boolean;
begin
  Inc(FOffset);
  Result:= FList.Count > FOffset;
end;

{ TListRecord<T>.TListRecordWrapper }

constructor TListRecord<T>.TListRecordWrapper.Create(const AList: TListRecord<T>);
begin
  FList:= @AList;
end;

function TListRecord<T>.TListRecordWrapper.GetEnumerator: TListRecordEnumerator;
begin
  Result.Create(FList^);
end;

initialization

  TFictiveSparseSectionComparer:= IComparer<TFictiveSparseSection>(@TFictiveSparseSectionComparerData);

end.
