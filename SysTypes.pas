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

function ValLongAnsi(const S: AnsiString; var Code: Integer): Integer; overload; inline;
function ValLongAnsi(S: PAnsiChar; var Code: Integer): Integer; overload;
function ValExtAnsi(const S: AnsiString; var Code: Integer): Extended; overload; inline;
function ValExtAnsi(S: PAnsiChar; var Code: Integer): Extended; overload;

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
function ValLongAnsi(S: PAnsiChar; var Code: Integer): Integer;
{$IF (Not DEFined(CPUX86))}
var
  I: Integer;
  Sign, Hex: Boolean;
begin
  Result := 0;
  if S = nil then begin
    Code:= 1;
    Exit;
  end;
  I := 0;
  Sign := False;
  Hex := False;
  while s[I] = ' ' do Inc(I);
  case s[I] of
    '$',
    'x',
    'X':
        begin
          Hex := True;
          Inc(I);
        end;
    '0':
        begin
          Hex := Ord(s[I + 1]) or $20 = Ord('x');
          if Hex then Inc(I, 2);
        end;
    '-':
        begin
          Sign := True;
          Inc(I);
        end;
    '+': Inc(I);
  end;
  if Hex then
    while S[I] <> #0 do
    begin
      if Result > (High(Result) div 16) then
      begin
        code := I + 1;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 16 + Ord(s[I]) - Ord('0');
        'a'..'f': Result := Result * 16 + Ord(s[I]) - Ord('a') + 10;
        'A'..'F': Result := Result * 16 + Ord(s[I]) - Ord('A') + 10;
      else
        code := I + 1;
        Exit;
      end;
      Inc(I);
    end
  else
    while S[I] <> #0 do
    begin
      if Result > (High(Result) div 10) then
      begin
        code := I + 1;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 10 + Ord(s[I]) - Ord('0');
      else
        code := I + 1;
        Exit;
      end;
      Inc(I);
    end;
  if Sign then
    Result := -Result;
  code := 0;
end;
{$ELSE}
asm
{       FUNCTION _ValLong( s: AnsiString; VAR code: Integer ) : Longint;        }
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }
{     <-EAX     Result                  }

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
        INC     ESI
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
        INC     ESI
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
        INC     ESI

        TEST    BL,BL
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        INC     ESI
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
        INC     ESI
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH

        MOV     BL,[ESI]
        INC     ESI
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
        INC     ESI

        TEST    BL,BL
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

function ValLongAnsi(const S: AnsiString; var Code: Integer): Integer;
begin
  Result:= ValLongAnsi(PAnsiChar(Pointer(S)), Code);
end;

function ValExtAnsi(S: PAnsiChar; var Code: Integer): Extended;
{$IF (Not DEFined(CPUX86))}
var
  Ch: AnsiChar;
  Digits, ExpValue: Integer;
  Neg, NegExp, Valid: Boolean;
  LocalCode: Integer;
begin
  if S = nil then
  begin
    Code:= 1;
    Exit;
  end;
  Result := 0.0;
  LocalCode := 0;
  Neg := False;
  NegExp := False;
  Valid := False;
  while S[LocalCode] = ' ' do
    Inc(LocalCode);
  Ch := S[LocalCode];
  if (Ch = '+') or (Ch = '-') then
  begin
    Inc(LocalCode);
    Neg := (Ch = '-');
  end;
  while True do
  begin
    Ch := S[LocalCode];
    Inc(LocalCode);
    if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
      Break;
    Result := (Result * 10) + Ord(Ch) - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = '.' then
  begin
    while True do
    begin
      Ch := S[LocalCode];
      Inc(LocalCode);
      if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
      begin
        if not Valid then {Starts with '.'}
        begin
          if Ch = #0 then
          begin
            Dec(LocalCode); {S = '.'}
            Valid := True; // SB: Added for compatibility with x86 asm version
          end;
        end;
        Break;
      end;
      Result := (Result * 10) + Ord(Ch) - Ord('0');
      Dec(Digits);
      Valid := True;
    end;
  end;
  ExpValue := 0;
  if (Ord(Ch) or $20) = Ord('e') then
    begin {Ch in ['E','e']}
      Valid := False;
      Ch := S[LocalCode];
      if (Ch = '+') or (Ch = '-') then
      begin
        Inc(LocalCode);
        NegExp := (Ch = '-');
      end;
      while True do
      begin
        Ch := S[LocalCode];
        Inc(LocalCode);
        if not ((Ord(Ch) >= Ord('0')) and (Ord(Ch) <= Ord('9'))) then
          Break;
        ExpValue := (ExpValue * 10) + Ord(Ch) - Ord('0');
        Valid := True;
      end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    Result := Power10(Result, Digits);
  if Neg then
    Result := -Result;
  if Valid and (Ch = #0) then
    LocalCode := 0;
  Code:= LocalCode;
end;
{$ELSE CPUX86}
const
  Ten: Double = 10.0;
asm
// -> EAX Pointer to string
//  EDX Pointer to code result
// <- FST(0)  Result

      PUSH    EBX
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      MOV     EBX,EAX
      POP     EAX
{$ELSE}
      XOR     EBX,EBX
{$ENDIF}
      PUSH    ESI
      PUSH    EDI

      PUSH    EBX     // SaveGOT = ESP+8
      MOV     ESI,EAX
      PUSH    EAX     // save for the error case

      FLDZ
      XOR     EAX,EAX
      XOR     EBX,EBX
      XOR     EDI,EDI

      PUSH    EBX     // temp to get digs to fpu

      TEST    ESI,ESI
      JE      @@empty

@@blankLoop:
      MOV     BL,[ESI]
      INC     ESI
      CMP     BL,' '
      JE      @@blankLoop

@@endBlanks:
      MOV     CH,0
      CMP     BL,'-'
      JE      @@minus
      CMP     BL,'+'
      JE      @@plus
      JMP     @@firstDigit

@@minus:
      INC     CH
@@plus:
      MOV     BL,[ESI]
      INC     ESI

@@firstDigit:
      TEST    BL,BL
      JE      @@error

      MOV     EDI,[ESP+8]     // SaveGOT

@@digLoop:
      SUB     BL,'0'
      CMP     BL,9
      JA      @@dotExp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]

      MOV     BL,[ESI]
      INC     ESI

      TEST    BL,BL
      JNE     @@digLoop
      JMP     @@prefinish

@@dotExp:
      CMP     BL,'.' - '0'
      JNE     @@exp

      MOV     BL,[ESI]
      INC     ESI

      TEST    BL,BL
      JE      @@prefinish

//  EDI = SaveGot
@@fracDigLoop:
      SUB     BL,'0'
      CMP     BL,9
      JA      @@exp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      DEC     EAX

      MOV     BL,[ESI]
      INC     ESI

      TEST    BL,BL
      JNE     @@fracDigLoop

@@prefinish:
      XOR     EDI,EDI
      JMP     @@finish

@@exp:
      CMP     BL,'E' - '0'
      JE      @@foundExp
      CMP     BL,'e' - '0'
      JNE     @@error
@@foundExp:
      MOV     BL,[ESI]
      INC     ESI
      MOV     AH,0
      CMP     BL,'-'
      JE      @@minusExp
      CMP     BL,'+'
      JE      @@plusExp
      JMP     @@firstExpDigit
@@minusExp:
      INC     AH
@@plusExp:
      MOV     BL,[ESI]
      INC     ESI
@@firstExpDigit:
      SUB     BL,'0'
      CMP     BL,9
      JA      @@error
      MOV     EDI,EBX
      MOV     BL,[ESI]
      INC     ESI
      TEST    BL,BL
      JZ      @@endExp
@@expDigLoop:
      SUB    BL,'0'
      CMP    BL,9
      JA     @@error
      LEA    EDI,[EDI+EDI*4]
      ADD    EDI,EDI
      ADD    EDI,EBX
      MOV    BL,[ESI]
      INC    ESI
      TEST   BL,BL
      JNZ    @@expDigLoop
@@endExp:
      DEC    AH
      JNZ    @@expPositive
      NEG    EDI
@@expPositive:
      MOVSX  EAX,AL

@@finish:
      ADD    EAX,EDI
      PUSH   EDX
      PUSH   ECX
      CALL   Power10
      POP    ECX
      POP    EDX

      DEC    CH
      JE     @@negate

@@successExit:
      ADD    ESP,12   // pop temp and saved copy of string pointer

      XOR    ESI,ESI   // signal no error to caller

@@exit:
      MOV    [EDX],ESI

      POP    EDI
      POP    ESI
      POP    EBX
      RET

@@negate:
      FCHS
      JMP    @@successExit

@@empty:
      INC    ESI

@@error:
      POP    EAX
      POP    EBX
      SUB    ESI,EBX
      ADD    ESP,4
      JMP    @@exit
end;
{$ENDIF CPUX86}

function ValExtAnsi(const S: AnsiString; var Code: Integer): Extended;
begin
  Result:= ValExtAnsi(PAnsiChar(Pointer(S)), Code);
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
