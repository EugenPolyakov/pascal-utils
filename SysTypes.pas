unit SysTypes;

interface

uses
  System.SysUtils, System.Generics.Collections, Winapi.Windows, Classes;

type
  // System string type, must identical with System.StrRec
  // Keep in sync.
  StrRecXE7Header = record
  {$IF defined(CPUX64)}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$ENDIF}
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;
  PStrRecXE7A = ^StrRecXE7A;
  StrRecXE7A = packed record
    Header: StrRecXE7Header;
    str: array [0..0] of AnsiChar;
  end;
  PStrRecXE7W = ^StrRecXE7W;
  StrRecXE7W = packed record
    Header: StrRecXE7Header;
    str: array [0..0] of WideChar;
  end;

  //Описание строки в формате Delphi 7
  StrRec7Header = record
    refCnt: Longint;
    length: Longint;
  end;
  PStrRec7 = ^StrRec7;
  StrRec7 = packed record
    Header: StrRec7Header;
    str: array [0..0] of AnsiChar;
  end;

  StrRecA = StrRecXE7A;
  PStrRecA = ^StrRecA;
  StrRecW = StrRecXE7W;
  PStrRecW = ^StrRecW;
  StrRecHeader = StrRecXE7Header;

  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPUX64}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$ENDIF}
    RefCnt: LongInt;
    Length: NativeInt;
  end;

  TOpenList<T> = class
  private
  protected
  public
  end;

  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: Pointer;
    ReturnAddr: Pointer;
  end;

  NT_TIB32 = packed record
    ExceptionList: DWORD;
    StackBase: DWORD;
    StackLimit: DWORD;
    SubSystemTib: DWORD;
    case Integer of
      0 : (
        FiberData: DWORD;
        ArbitraryUserPointer: DWORD;
        Self: DWORD;
      );
      1 : (
        Version: DWORD;
      );
  end;

  NT_TIB64 = packed record
    ExceptionList: ULONGLONG;
    StackBase: ULONGLONG;
    StackLimit: ULONGLONG;
    SubSystemTib: ULONGLONG;
    case Integer of
      0 : (
        FiberData: ULONGLONG;
        ArbitraryUserPointer: ULONGLONG;
        Self: ULONGLONG;
      );
      1 : (
        Version: DWORD;
      );
  end;

  TConstGenerator = class
    class function GetMemoryLengthForArray(typInfo: PDynArrayTypeInfo; p: PDynArrayRec): Integer;
    class function CopyArray(typInfo: PDynArrayTypeInfo; source, dest: PDynArrayRec): Integer;
    class function CreateConstArray<T>(out Arr: PDynArrayRec; const Source: TArray<T>): TArray<T>; overload; static;
    class function CreateConstArray<T>(var Arr: TDynArrayRec; const Source: TArray<T>): TArray<T>; overload; static;
    class function CreateConstString(var NewConst: StrRecA; const S: AnsiString): AnsiString; overload; static;
    class function CreateConstString(out NewConst: PStrRecA; const S: AnsiString): AnsiString; overload; static;
    class function CreateConstString(var NewConst: StrRecW; const S: UnicodeString): UnicodeString; overload; static;
    class function CreateConstString(out NewConst: PStrRecW; const S: UnicodeString): UnicodeString; overload; static;
  end;

  TRecordFinalizator = procedure of object;

  TAutoFinalizedRecord = record
  private
    FVtable: Pointer;
    FRefCount: Integer;
    FFinalizer: IInterface;
    FRecordFinalizator: TRecordFinalizator;
  public
    procedure InitFinalizator(ARecordFinalizator: TRecordFinalizator);
    procedure ChangeFinalizator(ARecordFinalizator: TRecordFinalizator);
    function IsInitialized: Boolean; inline;
  end;
  PAutoFinalizedRecord = ^TAutoFinalizedRecord;

  TSystemTypesHelpers = class
  public
    class function IsConstant<T>(const AValue: TArray<T>): Boolean; overload; static; inline;
    class function IsConstant(const AValue: UnicodeString): Boolean; overload; static; inline;
    class function IsConstant(const AValue: AnsiString): Boolean; overload; static; inline;
    class procedure CopyArrayAsConst<T>(var Dest: TArray<T>; const AValue: TArray<T>); static; inline;
    class function ConvertFromString<T>(const AValue: string): T; overload; static; inline;
    class procedure ConvertFromString<T>(const AValue: string; var Result: T); overload; static; inline;
  end;

function GetNumberOfSetBitsAllValues(L: LongWord): LongWord; inline;
function GetNumberOfSetBits32(L: LongWord): LongWord; inline;
function GetIndexOfSetBit(L: LongWord): LongInt; assembler;
function GetMaxIndexOfSetBit(L: LongWord): LongInt; assembler;
function IsPOTValue(L: LongWord; ZeroResult: Boolean = False): Boolean;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
function TAutoFinalizedRecord_Addref(inst: PAutoFinalizedRecord): Integer; stdcall;
function TAutoFinalizedRecord_Release(inst: PAutoFinalizedRecord): Integer; stdcall;
function NopAddref(inst: PAutoFinalizedRecord): Integer; stdcall;
function NopRelease(inst: PAutoFinalizedRecord): Integer; stdcall;

function ValAnsi(const S: AnsiString; var Code: Integer): Integer; overload; inline;
function ValAnsi(S: PAnsiChar; var Code: Integer): Integer; overload;

implementation

uses System.Types;

const
  Finalizer_Vtable: array[0..2] of Pointer =
  (
    @NopQueryInterface,
    @TAutoFinalizedRecord_Addref,
    @TAutoFinalizedRecord_Release
  );

// Hex : ( '$' | 'X' | 'x' | '0X' | '0x' ) [0-9A-Fa-f]*
// Dec : ( '+' | '-' )? [0-9]*
function ValAnsi(S: PAnsiChar; var Code: Integer): Integer;
{$IFDEF CPUX64}
var
  I: Integer;
  Dig: Integer;
  Sign: Boolean;
  Empty: Boolean;
begin
  I := 0;
  Sign := False;
  Result := 0;
  Dig := 0;
  Empty := True;

  if S = '' then
  begin
    Code := 1;
    Exit;
  end;
  while S[I] = ' ' do
    Inc(I);

  if S[I] = '-' then
  begin
    Sign := True;
    Inc(I);
  end
  else if S[I] = '+' then
    Inc(I);
  // Hex
  if ((S[I] = '0') and ((S[I+1] = 'X') or (S[I+1] = 'x'))) or
      (S[I] = '$') or
      (S[I] = 'X') or
      (S[I] = 'x') then
  begin
    if S[I] = '0' then
      Inc(I);
    Inc(I);
    while True do
    begin
      case S[I] of
       '0'..'9': Dig := Ord(S[I]) - Ord('0');
       'A'..'F': Dig := Ord(S[I]) - Ord('A') + 10;
       'a'..'f': Dig := Ord(S[I]) - Ord('a') + 10;
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Longint) shr 3)) then
        Break;
      Result := Result shl 4 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
  end
  // Decimal
  else
  begin
    while True do
    begin
      case S[I] of
        '0'..'9': Dig := Ord(S[I]) - Ord('0');
      else
        Break;
      end;
      if (Result < 0) or (Result > (High(Longint) div 10)) then
        Break;
      Result := Result*10 + Dig;
      Inc(I);
      Empty := False;
    end;

    if Sign then
      Result := - Result;
    if (Result <> 0) and (Sign <> (Result < 0)) then
      Dec(I);
  end;

  if ((S[I] <> Char(#0)) or Empty) then
    Code := I + 1
  else
    Code := 0;
end;
{$ELSE}
asm
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        PUSH    EAX             { save for the error case       }

        TEST    EAX,EAX
        JE      @@empty

        XOR     EAX,EAX
        XOR     EBX,EBX
        MOV     EDI,07FFFFFFFH / 10     { limit }

@@blankLoop:
        MOV     BL,[ESI]
        ADD     ESI, 1
        CMP     BL,' '
        JE      @@blankLoop

@@endBlanks:
        MOV     CH,0
        CMP     BL,'-'
        JE      @@minus
        CMP     BL,'+'
        JE      @@plus

@@checkDollar:
        CMP     BL,'$'
        JE      @@dollar

        CMP     BL, 'x'
        JE      @@dollar
        CMP     BL, 'X'
        JE      @@dollar
        CMP     BL, '0'
        JNE     @@firstDigit
        MOV     BL, [ESI]
        ADD     ESI, 1
        CMP     BL, 'x'
        JE      @@dollar
        CMP     BL, 'X'
        JE      @@dollar
        TEST    BL, BL
        JE      @@endDigits
        JMP     @@digLoop

@@firstDigit:
        TEST    BL,BL
        JE      @@error

@@digLoop:
        SUB     BL,'0'
        CMP     BL,9
        JA      @@error
        CMP     EAX,EDI         { value > limit ?       }
        JA      @@overFlow
        LEA     EAX,[EAX+EAX*4]
        ADD     EAX,EAX
        ADD     EAX,EBX         { fortunately, we can't have a carry    }
        MOV     BL,[ESI]
        ADD     ESI, 1
        TEST    BL,BL
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        ADD     ESI, 1
        JMP     @@error

@@negate:
        NEG     EAX
        JLE     @@successExit
        JS      @@successExit           { to handle 2**31 correctly, where the negate overflows }

@@error:
@@overFlow:
        POP     EBX
        SUB     ESI,EBX
        JMP     @@exit

@@minus:
        INC     CH
@@plus:
        MOV     BL,[ESI]
        ADD     ESI, 1
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH
        MOV     BL,[ESI]
        ADD     ESI, 1
        TEST    BL,BL
        JZ      @@empty

@@hDigLoop:
        CMP     BL,'a'
        JB      @@upper
        SUB     BL,'a' - 'A'
@@upper:
        SUB     BL,'0'
        CMP     BL,9
        JBE     @@digOk
        SUB     BL,'A' - '0'
        CMP     BL,5
        JA      @@error
        ADD     BL,10
@@digOk:
        CMP     EAX,EDI
        JA      @@overFlow
        SHL     EAX,4
        ADD     EAX,EBX
        MOV     BL,[ESI]
        ADD     ESI, 1
        TEST    BL,BL
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        SHR     ESI, 1
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

function ValAnsi(const S: AnsiString; var Code: Integer): Integer;
begin
  Result:= ValAnsi(PAnsiChar(S), Code);
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function TAutoFinalizedRecord_Addref(inst: PAutoFinalizedRecord): Integer; stdcall;
begin
  Result:= AtomicIncrement(inst.FRefCount);
  if Result > 1 then
    raise ENotSupportedException.Create('Don''t use copy of this structure and use only as var or const parameters of functions');
end;

function TAutoFinalizedRecord_Release(inst: PAutoFinalizedRecord): Integer; stdcall;
begin
  Result:= AtomicDecrement(inst.FRefCount);
  if (Result = 0) and Assigned(inst.FRecordFinalizator) then
    inst.FRecordFinalizator;
end;

function NopAddref(inst: PAutoFinalizedRecord): Integer; stdcall;
begin
  Result:= -1;
end;

function NopRelease(inst: PAutoFinalizedRecord): Integer; stdcall;
begin
  Result:= -1;
end;

{ TAutoFinalizedRecord }

procedure TAutoFinalizedRecord.ChangeFinalizator(
  ARecordFinalizator: TRecordFinalizator);
begin
  if FFinalizer <> nil then
    FRecordFinalizator:= ARecordFinalizator
  else
    InitFinalizator(ARecordFinalizator);
end;

procedure TAutoFinalizedRecord.InitFinalizator(
  ARecordFinalizator: TRecordFinalizator);
begin
  FRefCount:= 0;
  FRecordFinalizator:= ARecordFinalizator;
  FVtable:= @Finalizer_Vtable;
  FFinalizer:= IInterface(@Self);
end;

function TAutoFinalizedRecord.IsInitialized: Boolean;
begin
  Result:= FFinalizer <> nil;
end;

function IsPOTValue(L: LongWord; ZeroResult: Boolean): Boolean;
begin
  if L <> 0 then
    Result:= L = 1 shl GetMaxIndexOfSetBit(L)
  else
    Result:= ZeroResult;
end;

function GetMaxIndexOfSetBit(L: LongWord): LongInt;
{$IFDEF CPUX64}
asm
  BSR ECX, ECX
  JZ @null
  MOV EAX, ECX
  RET
@null:
  MOV EAX, -1
end;
{$ELSE}
asm
  BSR EAX, EAX
  JZ @null
  RET
@null:
  MOV EAX, -1
end;
{$ENDIF}

function GetIndexOfSetBit(L: LongWord): LongInt;
{$IFDEF CPUX64}
asm
  BSF ECX, ECX
  JZ @null
  MOV EAX, ECX
  RET
@null:
  MOV EAX, -1
end;
{$ELSE}
asm
  BSF EAX, EAX
  JZ @null
  RET
@null:
  MOV EAX, -1
end;
{$ENDIF}

function GetNumberOfSetBitsAllValues(L: LongWord): LongWord;
begin
  //Result:= L shr 1 and $55555555 + L and $55555555;
  Result:= L - L shr 1 and $55555555;
  Result:= Result shr 2 and $33333333 + Result and $33333333;
  //Result:= Result shr 4 and $0F0F0F0F + Result and $0F0F0F0F;
  {Result:= (Result + Result shr 4) and $0F0F0F0F;
  Result:= (Result + Result shr 8) and $00FF00FF;
  Result:= (Result + Result shr 16) and $FFFF; }
  Result:= ((Result + Result shr 4) and $0F0F0F0F) * $01010101;
end;

function GetNumberOfSetBits32(L: LongWord): LongWord;
begin
  Result:= GetNumberOfSetBitsAllValues(L) shr $18;
end;

{ TConstGenerator }

class function TConstGenerator.CopyArray(typInfo: PDynArrayTypeInfo; source,
  dest: PDynArrayRec): Integer;
var elType: ^PDynArrayTypeInfo;
    len: Integer;
    inlineArray: ^PDynArrayRec;
    newSource: PDynArrayRec;
    sourceOfs: PPointer;
    I: Integer;
begin
  dest^:= source^;
  dest.RefCnt:= -1;
  len:= dest.Length;
  Inc(PByte(typInfo), PDynArrayTypeInfo(typInfo).name);
  elType:= Pointer(typInfo.elType);
  if elType <> nil then begin
    Inc(dest);
    Inc(source);
    sourceOfs:= Pointer(source);
    inlineArray:= Pointer(dest);
    Result:= len * SizeOf(Pointer);
    for I := 0 to len - 1 do begin
      inlineArray^:= Pointer(Integer(dest) + Result);
      newSource:= sourceOfs^;
      Dec(newSource);
      Inc(Result, CopyArray(elType^, newSource, inlineArray^));
      Inc(sourceOfs);
      Inc(inlineArray^);
      Inc(inlineArray);
    end;
  end else begin
    Inc(dest);
    Inc(source);
    Move(source^, dest^, len * typInfo.elSize);
    Result:= len * typInfo.elSize;
  end;
  Inc(Result, SizeOf(TDynArrayRec));
end;

class function TConstGenerator.CreateConstArray<T>(out Arr: PDynArrayRec;
  const Source: TArray<T>): TArray<T>;
var typInfo: PDynArrayTypeInfo;
    fullSize, len: Integer;
    old: PDynArrayRec;
begin
  typInfo:= TypeInfo(TArray<T>);
  old:= Pointer(Source);
  Dec(old);
  fullSize:= GetMemoryLengthForArray(typInfo, old);
  Arr:= nil;
  GetMem(Arr, fullSize);
  Result:= CreateConstArray<T>(Arr^, Source);
end;

class function TConstGenerator.CreateConstArray<T>(var Arr: TDynArrayRec;
  const Source: TArray<T>): TArray<T>;
var old: PDynArrayRec;
begin
  old:= Pointer(Source);
  Dec(old);
  CopyArray(TypeInfo(TArray<T>), old, @Arr);
  Result:= nil;
  old:= @Arr;
  Inc(old);
  Pointer(Result):= old;
end;

class function TConstGenerator.CreateConstString(out NewConst: PStrRecA; const S: AnsiString): AnsiString;
begin
  if S = '' then begin
    NewConst:= nil;
    Result:= '';
  end else begin
    GetMem(NewConst, SizeOf(StrRecHeader) + Length(S) + 1);
    Result:= CreateConstString(NewConst^, S);
  end;
end;

class function TConstGenerator.CreateConstString(out NewConst: PStrRecW; const S: UnicodeString): UnicodeString;
begin
  if S = '' then begin
    NewConst:= nil;
    Result:= '';
  end else begin
    GetMem(NewConst, SizeOf(StrRecHeader) + (Length(S) + 1) * SizeOf(WideChar));
    Result:= CreateConstString(NewConst^, S);
  end;
end;

class function TConstGenerator.CreateConstString(var NewConst: StrRecA; const S: AnsiString): AnsiString;
var o: PStrRecA;
begin
  o:= Pointer(NativeInt(S) - SizeOf(StrRecHeader));
  NewConst.Header:= o.Header;
  NewConst.Header.refCnt:= -1;
  Move(o.str[0], NewConst.str[0], (NewConst.Header.length + 1) * SizeOf(AnsiChar));
  Result:= '';
  Pointer(Result):= @NewConst.str[0];
end;

class function TConstGenerator.CreateConstString(var NewConst: StrRecW; const S: UnicodeString): UnicodeString;
var o: PStrRecW;
begin
  o:= Pointer(NativeInt(S) - SizeOf(StrRecHeader));
  NewConst.Header:= o.Header;
  NewConst.Header.refCnt:= -1;
  Move(o.str[0], NewConst.str[0], (NewConst.Header.length + 1) * SizeOf(WideChar));
  Result:= '';
  Pointer(Result):= @NewConst.str[0];
end;

class function TConstGenerator.GetMemoryLengthForArray(
  typInfo: PDynArrayTypeInfo; p: PDynArrayRec): Integer;
var
  I, len: Integer;
  arrData: PPointer;
  elType: ^PDynArrayTypeInfo;
begin
  Result:= SizeOf(TDynArrayRec);
  Inc(PByte(typInfo), PDynArrayTypeInfo(typInfo).name);
  Inc(Result, p.Length * typInfo.elSize);
  elType:= Pointer(typInfo.elType);
  if elType <> nil then begin
    len:= p.Length;
    Inc(p);
    arrData:= Pointer(p);
    for I := 0 to len - 1 do begin
      p:= arrData^;
      Dec(p);
      Inc(Result, GetMemoryLengthForArray(elType^, p));
      Inc(arrData);
    end;
  end;
end;

{ TSystemTypesHelpers }

class procedure TSystemTypesHelpers.CopyArrayAsConst<T>(var Dest: TArray<T>;
  const AValue: TArray<T>);
begin
  if IsConstant<T>(AValue) then begin
    Dest:= nil;
    Pointer(Dest):= Pointer(AValue)
  end else
    Dest:= AValue;
end;

class procedure TSystemTypesHelpers.ConvertFromString<T>(const AValue: string;
  var Result: T);
var e: Extended absolute Result;
    d: Double absolute Result;
    s: Single absolute Result;
    r: Real absolute Result;
    b: Byte absolute Result;
    w: Word absolute Result;
    l: LongWord absolute Result;
    si: SmallInt absolute Result;
    short: ShortInt absolute Result;
    li: LongInt absolute Result;
    i64: Int64 absolute Result;
    ui64: UInt64 absolute Result;
    ss: string absolute Result;
    ansi: AnsiString absolute Result;
begin
  if TypeInfo(T) = TypeInfo(Byte) then begin
    b:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(Word) then begin
    w:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(LongWord) then begin
    l:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(UInt64) then begin
    ui64:= StrToUInt64Def(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(SmallInt) then begin
    si:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(ShortInt) then begin
    short:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(LongInt) then begin
    li:= StrToIntDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(Int64) then begin
    i64:= StrToInt64Def(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(string) then begin
    ss:= AValue
  end else if TypeInfo(T) = TypeInfo(AnsiString) then begin
    ansi:= ANsiString(AValue)
  end else if TypeInfo(T) = TypeInfo(Double) then begin
    d:= StrToFloatDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(Single) then begin
    s:= StrToFloatDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(Extended) then begin
    e:= StrToFloatDef(AValue, 0);
  end else if TypeInfo(T) = TypeInfo(Real) then begin
    r:= StrToFloatDef(AValue, 0);
  end else
    Result:= Default(T);
end;

class function TSystemTypesHelpers.ConvertFromString<T>(
  const AValue: string): T;
begin
  ConvertFromString<T>(AValue, Result);
end;

class function TSystemTypesHelpers.IsConstant(
  const AValue: UnicodeString): Boolean;
begin
  Result:= PStrRecW(NativeInt(AValue) - SizeOf(StrRecHeader)).Header.refCnt = -1;
end;

class function TSystemTypesHelpers.IsConstant(
  const AValue: AnsiString): Boolean;
begin
  Result:= PStrRecA(NativeInt(AValue) - SizeOf(StrRecHeader)).Header.refCnt = -1;
end;

class function TSystemTypesHelpers.IsConstant<T>(
  const AValue: TArray<T>): Boolean;
begin
  Result:= PDynArrayRec(NativeInt(@AValue[0]) - SizeOf(TDynArrayRec)).RefCnt = -1;
end;

end.
