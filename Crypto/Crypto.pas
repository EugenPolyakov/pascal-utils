unit Crypto;

interface

uses System.SysUtils, System.Classes;

type
  TSalsa20Key = array [0..15] of LongWord;
  TSalsa20Data = record
    Key: TSalsa20Key;
    Rounds: Integer;
    procedure Initialize(AKey: array of Byte; Vector: array of LongWord; ARounds: Integer);
    procedure Decrypt(const Source; var Dest; Len: Integer);
  end;
  TMD5Hash = record
    case Integer of
      0: (bytes: array [0..15] of Byte);
      1: (l: array [0..3] of LongWord);
      2: (A, B, C, D: LongWord);
      3: (AsGUID: TGUID);
  end;
  TMD5Block = array [0..15] of LongWord;

  TMD5HashAlg = record
  private
    FBitLength: UInt64;
    FHash: TMD5Hash;
  public
    procedure Initialize; overload;
    procedure Initialize(const Start: TMD5Hash); overload;
    function Finalize(const Buffer; Length: Integer): TMD5Hash;
    procedure AppendBuffer(const Buffer: TMD5Block);
    function GetStreamHash(AStream: TStream): TMD5Hash;
  end;

function Rol32(Val: LongWord; k: Byte): LongWord; inline;
function Align(Val, a: LongInt): LongInt; inline; overload;
function Align(Val, a: Int64): Int64; inline; overload;
procedure Hashlittle2(var Buff; Length: LongWord; var Prim, Sec: LongWord);
function GetMD5Hash(const Buff; Length: Integer): TMD5Hash;
function GetMD5BlockHash(const Buff: TMD5Block; Init: TMD5Hash): TMD5Hash;
function BinToHex(const Buff; Length: Integer): string; overload;
function BinToHexLower(const Buff; Length: Integer): string; overload;
function BinToHex(const Buff: TMD5Hash): string; overload;
function BinToHexLower(const Buff: TMD5Hash): string; overload;
function BinToHex(const Buff: TArray<Byte>): string; overload;
function BinToHexLower(const Buff: TArray<Byte>): string; overload;
function HexToBin(const Str: string): TArray<Byte>;
function GetCRC16Hash(const Buff; Length: LongWord; Start: Word = $FFFF): Word; overload;
function GetCRC16Hash(const Buff: TArray<Byte>; Start: Word = $FFFF): Word; overload;

const
  CRC16_Table : array [0..255] of Word = (
    $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0
  );

implementation

function GetCRC16Hash(const Buff: TArray<Byte>; Start: Word): Word;
begin
  Result:= GetCRC16Hash(Buff[0], Length(Buff), Start);
end;

function GetCRC16Hash(const Buff; Length: LongWord; Start: Word): Word;
var
  I: Integer;
  data: array [0..0] of byte absolute Buff;
begin
  Result:= Start;
  for I := 0 to Length - 1 do
    Result:= (Result shl 8) xor CRC16_Table[(Result shr 8) xor data[I]];
end;

function HexToBin(const Str: string): TArray<Byte>;
var i: Integer;
    b: Byte;
begin
  SetLength(Result, Length(Str) div 2);
  for i := 1 to Length(Str) do begin
    case Str[i] of
      'a'..'f': b:= Ord(Str[i]) - Ord('a') + $A;
      'A'..'F': b:= Ord(Str[i]) - Ord('A') + $A;
      '0'..'9': b:= Ord(Str[i]) - Ord('0') + $0;
    else
      Break;
    end;
    if i mod 2 = 1 then
      Result[(i - 1) div 2]:= b shl 4
    else
      Result[(i - 1) div 2]:= Result[(i - 1) div 2] + b;
  end;
end;

function BinToHex(const Buff; Length: Integer): string;
const NumChars: PWideChar = '0123456789ABCDEF';
var i: Integer;
    b: array [0..0] of Byte absolute Buff;
begin
  Result:= '';
  SetLength(Result, Length * 2);
  for i := 0 to Length - 1 do begin
    Result[i * 2 + 1]:= NumChars[b[i] and $F0 shr 4];
    Result[i * 2 + 2]:= NumChars[b[i] and $F];
  end;
end;

function BinToHexLower(const Buff; Length: Integer): string;
const NumChars: PWideChar = '0123456789abcdef';
var i: Integer;
    b: array [0..0] of Byte absolute Buff;
begin
  Result:= '';
  SetLength(Result, Length * 2);
  for i := 0 to Length - 1 do begin
    Result[i * 2 + 1]:= NumChars[b[i] and $F0 shr 4];
    Result[i * 2 + 2]:= NumChars[b[i] and $F];
  end;
end;

function BinToHex(const Buff: TMD5Hash): string;
begin
  Result:= BinToHex(Buff, SizeOf(Buff));
end;

function BinToHexLower(const Buff: TMD5Hash): string;
begin
  Result:= BinToHexLower(Buff, SizeOf(Buff));
end;

function BinToHex(const Buff: TArray<Byte>): string;
begin
  Result:= BinToHex(Buff[0], Length(Buff));
end;

function BinToHexLower(const Buff: TArray<Byte>): string;
begin
  Result:= BinToHexLower(Buff[0], Length(Buff));
end;

function Rol32(Val: LongWord; k: Byte): LongWord;
begin
  Result:= (Val shl k) or (Val shr (32 - k));
end;

function Align(Val, a: LongInt): LongInt;
begin
  Result:= (Val + (a - 1));
  Result:= Result - (Result mod a);
end;

function Align(Val, a: Int64): Int64;
begin
  Result:= (Val + (a - 1));
  Result:= Result - (Result mod a);
end;

const
  MD4_INIT_VALUES: array [0..3] of LongWord = (
    $67452301, $EFCDAB89, $98BADCFE, $10325476);

  MD5_SINE : array[1..64] of LongWord = (
   { Round 1. }
   $d76aa478, $e8c7b756, $242070db, $c1bdceee, $f57c0faf, $4787c62a,
   $a8304613, $fd469501, $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
   $6b901122, $fd987193, $a679438e, $49b40821,
   { Round 2. }
   $f61e2562, $c040b340, $265e5a51, $e9b6c7aa, $d62f105d, $02441453,
   $d8a1e681, $e7d3fbc8, $21e1cde6, $c33707d6, $f4d50d87, $455a14ed,
   $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a,
   { Round 3. }
   $fffa3942, $8771f681, $6d9d6122, $fde5380c, $a4beea44, $4bdecfa9,
   $f6bb4b60, $bebfbc70, $289b7ec6, $eaa127fa, $d4ef3085, $04881d05,
   $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
   { Round 4. }
   $f4292244, $432aff97, $ab9423a7, $fc93a039, $655b59c3, $8f0ccc92,
   $ffeff47d, $85845dd1, $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
   $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391
  );

// 32-bit: Arg1=EAX, Arg2=DL
// 64-bit: Arg1=ECX, Arg2=DL
function ROL(const AVal: LongWord; AShift: Byte): LongWord; assembler;
asm
  {$IFDEF CPU64}
  mov eax, ecx
  {$ENDIF}
  mov  cl, dl
  rol  eax, cl
end;

// 32-bit: Arg1=EAX, Arg2=DL
// 64-bit: Arg1=ECX, Arg2=DL
function ROR(const AVal: LongWord; AShift: Byte): LongWord; assembler;
asm
  {$IFDEF CPU64}
  mov eax, ecx
  {$ENDIF}
  mov  cl, dl
  ror  eax, cl
end;

function GetMD5BlockHash(const Buff: TMD5Block; Init: TMD5Hash): TMD5Hash;
begin
  with Result do begin
    A := Init.A;
    B := Init.B;
    C := Init.C;
    D := Init.D;
    { Round 1 }
    { Note:
        (Buff and y) or ( (not Buff) and z)
      is equivalent to
        (((z xor y) and Buff) xor z)
      -HHellström }
    A := ROL(A + (((D xor C) and B) xor D) + Buff[ 0] + MD5_SINE[ 1],  7) + B;//
    D := ROL(D + (((C xor B) and A) xor C) + Buff[ 1] + MD5_SINE[ 2], 12) + A;//
    C := ROL(C + (((B xor A) and D) xor B) + Buff[ 2] + MD5_SINE[ 3], 17) + D;//
    B := ROL(B + (((A xor D) and C) xor A) + Buff[ 3] + MD5_SINE[ 4], 22) + C;//
    A := ROL(A + (((D xor C) and B) xor D) + Buff[ 4] + MD5_SINE[ 5],  7) + B;//
    D := ROL(D + (((C xor B) and A) xor C) + Buff[ 5] + MD5_SINE[ 6], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buff[ 6] + MD5_SINE[ 7], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buff[ 7] + MD5_SINE[ 8], 22) + C;
    A := ROL(A + (((D xor C) and B) xor D) + Buff[ 8] + MD5_SINE[ 9],  7) + B;
    D := ROL(D + (((C xor B) and A) xor C) + Buff[ 9] + MD5_SINE[10], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buff[10] + MD5_SINE[11], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buff[11] + MD5_SINE[12], 22) + C;
    A := ROL(A + (((D xor C) and B) xor D) + Buff[12] + MD5_SINE[13],  7) + B;
    D := ROL(D + (((C xor B) and A) xor C) + Buff[13] + MD5_SINE[14], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buff[14] + MD5_SINE[15], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buff[15] + MD5_SINE[16], 22) + C;

    { Round 2 }
    { Note:
        (Buff and z) or (y and (not z) )
      is equivalent to
        (((y xor Buff) and z) xor y)
      -HHellström }
    A := ROL(A + (C xor (D and (B xor C))) + Buff[ 1] + MD5_SINE[17],  5) + B;//
    D := ROL(D + (B xor (C and (A xor B))) + Buff[ 6] + MD5_SINE[18],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buff[11] + MD5_SINE[19], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buff[ 0] + MD5_SINE[20], 20) + C;//
    A := ROL(A + (C xor (D and (B xor C))) + Buff[ 5] + MD5_SINE[21],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buff[10] + MD5_SINE[22],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buff[15] + MD5_SINE[23], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buff[ 4] + MD5_SINE[24], 20) + C;//
    A := ROL(A + (C xor (D and (B xor C))) + Buff[ 9] + MD5_SINE[25],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buff[14] + MD5_SINE[26],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buff[ 3] + MD5_SINE[27], 14) + D;//
    B := ROL(B + (D xor (A and (C xor D))) + Buff[ 8] + MD5_SINE[28], 20) + C;
    A := ROL(A + (C xor (D and (B xor C))) + Buff[13] + MD5_SINE[29],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buff[ 2] + MD5_SINE[30],  9) + A;//
    C := ROL(C + (A xor (B and (D xor A))) + Buff[ 7] + MD5_SINE[31], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buff[12] + MD5_SINE[32], 20) + C;

    { Round 3. }
    A := ROL(A + (B xor C xor D) + Buff[ 5] + MD5_SINE[33],  4) + B;
    D := ROL(D + (A xor B xor C) + Buff[ 8] + MD5_SINE[34], 11) + A;
    C := ROL(C + (D xor A xor B) + Buff[11] + MD5_SINE[35], 16) + D;
    B := ROL(B + (C xor D xor A) + Buff[14] + MD5_SINE[36], 23) + C;
    A := ROL(A + (B xor C xor D) + Buff[ 1] + MD5_SINE[37],  4) + B;//
    D := ROL(D + (A xor B xor C) + Buff[ 4] + MD5_SINE[38], 11) + A;//
    C := ROL(C + (D xor A xor B) + Buff[ 7] + MD5_SINE[39], 16) + D;
    B := ROL(B + (C xor D xor A) + Buff[10] + MD5_SINE[40], 23) + C;
    A := ROL(A + (B xor C xor D) + Buff[13] + MD5_SINE[41],  4) + B;
    D := ROL(D + (A xor B xor C) + Buff[ 0] + MD5_SINE[42], 11) + A;//
    C := ROL(C + (D xor A xor B) + Buff[ 3] + MD5_SINE[43], 16) + D;//
    B := ROL(B + (C xor D xor A) + Buff[ 6] + MD5_SINE[44], 23) + C;
    A := ROL(A + (B xor C xor D) + Buff[ 9] + MD5_SINE[45],  4) + B;
    D := ROL(D + (A xor B xor C) + Buff[12] + MD5_SINE[46], 11) + A;
    C := ROL(C + (D xor A xor B) + Buff[15] + MD5_SINE[47], 16) + D;
    B := ROL(B + (C xor D xor A) + Buff[ 2] + MD5_SINE[48], 23) + C;//

    { Round 4. }
    A := ROL(A + ((B or not D) xor C) + Buff[ 0] + MD5_SINE[49],  6) + B;//
    D := ROL(D + ((A or not C) xor B) + Buff[ 7] + MD5_SINE[50], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buff[14] + MD5_SINE[51], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buff[ 5] + MD5_SINE[52], 21) + C;
    A := ROL(A + ((B or not D) xor C) + Buff[12] + MD5_SINE[53],  6) + B;
    D := ROL(D + ((A or not C) xor B) + Buff[ 3] + MD5_SINE[54], 10) + A;//
    C := ROL(C + ((D or not B) xor A) + Buff[10] + MD5_SINE[55], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buff[ 1] + MD5_SINE[56], 21) + C;//
    A := ROL(A + ((B or not D) xor C) + Buff[ 8] + MD5_SINE[57],  6) + B;
    D := ROL(D + ((A or not C) xor B) + Buff[15] + MD5_SINE[58], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buff[ 6] + MD5_SINE[59], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buff[13] + MD5_SINE[60], 21) + C;
    A := ROL(A + ((B or not D) xor C) + Buff[ 4] + MD5_SINE[61],  6) + B;//
    D := ROL(D + ((A or not C) xor B) + Buff[11] + MD5_SINE[62], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buff[ 2] + MD5_SINE[63], 15) + D;//
    B := ROL(B + ((C or not A) xor D) + Buff[ 9] + MD5_SINE[64], 21) + C;

    Inc(A, Init.A);
    Inc(B, Init.B);
    Inc(C, Init.C);
    Inc(D, Init.D);
  end;
end;

function GetMD5Hash(const Buff; Length: Integer): TMD5Hash;
var
  i: Integer;
  x: array [0..0] of Byte absolute Buff;
  lastBlock: TMD5Block;
  byteBlock: array [0..63] of Byte absolute lastBlock;
begin
  with Result do begin
    A := MD4_INIT_VALUES[0];
    B := MD4_INIT_VALUES[1];
    C := MD4_INIT_VALUES[2];
    D := MD4_INIT_VALUES[3];
  end;

  for i := 0 to Length div SizeOf(TMD5Block) - 1 do begin
    Move(x[i * SizeOf(TMD5Block)], lastBlock, SizeOf(TMD5Block));
    Result:= GetMD5BlockHash(lastBlock, Result);
  end;

  i:= Length mod SizeOf(lastBlock);
  FillChar(lastBlock, SizeOf(lastBlock), 0);
  Move(x[Length - i], lastBlock, i);
  byteBlock[i]:= $80;
  if i + 9 > SizeOf(lastBlock) then begin
    Result:= GetMD5BlockHash(lastBlock, Result);
    FillChar(lastBlock, SizeOf(lastBlock), 0);
  end;
  Length:= Length * 8;
  Move(Length, lastBlock[14], 4);
  Result:= GetMD5BlockHash(lastBlock, Result);
end;

procedure Mix(var a, b, c: LongWord); inline;
begin
  a:= (a - c) xor Rol32(c, 4);
  Inc(c, b);
  b:= (b - a) xor Rol32(a, 6);
  Inc(a, c);
  c:= (c - b) xor Rol32(b, 8);
  Inc(b, a);

  a:= (a - c) xor Rol32(c, 16);
  Inc(c, b);
  b:= (b - a) xor Rol32(a, 19);
  Inc(a, c);
  c:= (c - b) xor Rol32(b, 4);
  Inc(b, a);
end;

procedure Final_(var a, b, c: LongWord); inline;
begin
  c:= (c xor b) - Rol32(b, 14);
  a:= (a xor c) - Rol32(c, 11);
  b:= (b xor a) - Rol32(a, 25);

  c:= (c xor b) - Rol32(b, 16);
  a:= (a xor c) - Rol32(c, 4);
  b:= (b xor a) - Rol32(a, 14);

  c:= (c xor b) - Rol32(b, 24);
end;

procedure Hashlittle2(var Buff; Length: LongWord; var Prim, Sec: LongWord);
var b1: array[0..0] of packed record
        b1: Byte;
        w1: Word;
        b2: Byte;
      end absolute Buff;
    b4: array [0..0] of LongWord absolute Buff;
    b2: array [0..0] of packed record
        w1, w2: Word
      end absolute Buff;
    b0: array [0..0] of Byte absolute Buff;
    a, b, c: LongWord;
    i, nl: Integer;
begin
  a:= $DEADBEEF + Length + Prim;
  b:= a;
  c:= a + Sec;
  if Length <> 0 then begin
    nl:= Length div 4;
    case LongWord(@Buff) and 3 of
      0: begin //4-byte align
        for i := 0 to (Length + 11) div 12 - 2 do begin
          Inc(a, b4[i * 3 + 0]);
          Inc(b, b4[i * 3 + 1]);
          Inc(c, b4[i * 3 + 2]);
          Mix(a, b, c);
        end;

        case Length mod 12 of
          1: begin
            Inc(a, b0[Length - 1]);
          end;
          2: begin
            Inc(a, b2[nl].w1);
          end;
          3: begin
            Inc(a, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          4: begin
            Inc(a, b4[nl - 1]);
          end;
          5: begin
            Inc(a, b4[nl - 1]);
            Inc(b, b0[Length - 1]);
          end;
          6: begin
            Inc(a, b4[nl - 1]);
            Inc(b, b2[nl].w1);
          end;
          7: begin
            Inc(a, b4[nl - 1]);
            Inc(b, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          8: begin
            Inc(a, b4[nl - 2]);
            Inc(b, b4[nl - 1]);
          end;
          9: begin
            Inc(a, b4[nl - 2]);
            Inc(b, b4[nl - 1]);
            Inc(c, b0[Length - 1]);
          end;
          10: begin
            Inc(a, b4[nl - 2]);
            Inc(b, b4[nl - 1]);
            Inc(c, b2[nl].w1);
          end;
          11: begin
            Inc(a, b4[nl - 2]);
            Inc(b, b4[nl - 1]);
            Inc(c, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          0: begin
            Inc(a, b4[nl - 3]);
            Inc(b, b4[nl - 2]);
            Inc(c, b4[nl - 1]);
          end;
        end;
      end;
      2: begin //2-byte align
        for i := 0 to (Length + 11) div 12 - 2 do begin
          Inc(a, b2[i * 3 + 0].w1 + b2[i * 3 + 0].w2 shl 16);
          Inc(b, b2[i * 3 + 1].w1 + b2[i * 3 + 1].w2 shl 16);
          Inc(c, b2[i * 3 + 2].w1 + b2[i * 3 + 2].w2 shl 16);
          Mix(a, b, c);
        end;

        case Length mod 12 of
          1: begin
            Inc(a, b0[Length - 1]);
          end;
          2: begin
            Inc(a, b2[nl].w1);
          end;
          3: begin
            Inc(a, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          4: begin
            Inc(a, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
          end;
          5: begin
            Inc(a, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(b, b0[Length - 1]);
          end;
          6: begin
            Inc(a, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(b, b2[nl].w1);
          end;
          7: begin
            Inc(a, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(b, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          8: begin
            Inc(a, b2[nl - 2].w1 + b2[nl - 2].w2 shl 16);
            Inc(b, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
          end;
          9: begin
            Inc(a, b2[nl - 2].w1 + b2[nl - 2].w2 shl 16);
            Inc(b, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(c, b0[Length - 1]);
          end;
          10: begin
            Inc(a, b2[nl - 2].w1 + b2[nl - 2].w2 shl 16);
            Inc(b, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(c, b2[nl].w1);
          end;
          11: begin
            Inc(a, b2[nl - 2].w1 + b2[nl - 2].w2 shl 16);
            Inc(b, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
            Inc(c, b0[Length - 1] shl 16 + b2[nl].w1);
          end;
          0: begin
            Inc(a, b2[nl - 3].w1 + b2[nl - 3].w2 shl 16);
            Inc(b, b2[nl - 2].w1 + b2[nl - 2].w2 shl 16);
            Inc(c, b2[nl - 1].w1 + b2[nl - 1].w2 shl 16);
          end;
        end;
      end;
    else
      for i := 0 to (Length + 11) div 12 - 2 do begin
        Inc(a, b1[i * 3 + 0].b1 + b1[i * 3 + 0].w1 shl 8 + b1[i * 3 + 0].b2 shl 24);
        Inc(b, b1[i * 3 + 1].b1 + b1[i * 3 + 1].w1 shl 8 + b1[i * 3 + 1].b2 shl 24);
        Inc(c, b1[i * 3 + 2].b1 + b1[i * 3 + 2].w1 shl 8 + b1[i * 3 + 2].b2 shl 24);
        Mix(a, b, c);
      end;

      case Length mod 12 of
        1: begin
          Inc(a, b0[Length - 1]);
        end;
        2: begin
          Inc(a, b0[Length - 2] + b0[Length - 1] shl 8);
        end;
        3: begin
          Inc(a, b1[nl - 0].b1 + b1[nl - 0].w1 shl 8);
        end;
        4: begin
          Inc(a, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
        end;
        5: begin
          Inc(a, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(b, b0[Length - 1]);
        end;
        6: begin
          Inc(a, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(b, b0[Length - 2] + b0[Length - 1] shl 8);
        end;
        7: begin
          Inc(a, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(b, b1[nl - 0].b1 + b1[nl - 0].w1 shl 8);
        end;
        8: begin
          Inc(a, b1[nl - 2].b1 + b1[nl - 2].w1 shl 8 + b1[nl - 2].b2 shl 24);
          Inc(b, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
        end;
        9: begin
          Inc(a, b1[nl - 2].b1 + b1[nl - 2].w1 shl 8 + b1[nl - 2].b2 shl 24);
          Inc(b, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(c, b0[Length - 1]);
        end;
        10: begin
          Inc(a, b1[nl - 2].b1 + b1[nl - 2].w1 shl 8 + b1[nl - 2].b2 shl 24);
          Inc(b, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(c, b0[Length - 2] + b0[Length - 1] shl 8);
        end;
        11: begin
          Inc(a, b1[nl - 2].b1 + b1[nl - 2].w1 shl 8 + b1[nl - 2].b2 shl 24);
          Inc(b, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
          Inc(c, b1[nl - 0].b1 + b1[nl - 0].w1 shl 8);
        end;
        0: begin
          Inc(a, b1[nl - 3].b1 + b1[nl - 3].w1 shl 8 + b1[nl - 3].b2 shl 24);
          Inc(b, b1[nl - 2].b1 + b1[nl - 2].w1 shl 8 + b1[nl - 2].b2 shl 24);
          Inc(c, b1[nl - 1].b1 + b1[nl - 1].w1 shl 8 + b1[nl - 1].b2 shl 24);
        end;
      end;
    end;

    Final_(a, b, c);
  end;
  Prim:= c;
  Sec:= b;
end;

const
  key16: PAnsiChar = 'expand 16-byte k';
  key32: PAnsiChar = 'expand 32-byte k';

{ TSalsa20Data }

procedure TSalsa20Data.Decrypt(const Source; var Dest; Len: Integer);
var S1: array [0..0] of Byte absolute Source;
    S4: array [0..0] of LongWord absolute Source;
    D1: array [0..0] of Byte absolute Dest;
    D4: array [0..0] of LongWord absolute Dest;
    i: Integer;
    nKey: TSalsa20Key;
    nKey1: array [0..$40-1] of Byte absolute nKey;
begin
{$IFDEF DEBUG}
  if Len > $10 * 4 then
    raise Exception.Create('Error Message');
{$ENDIF}
  nKey:= Key;
  for i := 1 to Rounds div 2 do begin
    nKey[$04] := nKey[$04] xor Rol32((nKey[$00] + nKey[$0C]), $07);
    nKey[$08] := nKey[$08] xor Rol32((nKey[$04] + nKey[$00]), $09);
    nKey[$0C] := nKey[$0C] xor Rol32((nKey[$08] + nKey[$04]), $0D);
    nKey[$00] := nKey[$00] xor Rol32((nKey[$0C] + nKey[$08]), $12);

    nKey[$09] := nKey[$09] xor Rol32((nKey[$05] + nKey[$01]), $07);
    nKey[$0D] := nKey[$0D] xor Rol32((nKey[$09] + nKey[$05]), $09);
    nKey[$01] := nKey[$01] xor Rol32((nKey[$0D] + nKey[$09]), $0D);
    nKey[$05] := nKey[$05] xor Rol32((nKey[$01] + nKey[$0D]), $12);

    nKey[$0E] := nKey[$0E] xor Rol32((nKey[$0A] + nKey[$06]), $07);
    nKey[$02] := nKey[$02] xor Rol32((nKey[$0E] + nKey[$0A]), $09);
    nKey[$06] := nKey[$06] xor Rol32((nKey[$02] + nKey[$0E]), $0D);
    nKey[$0A] := nKey[$0A] xor Rol32((nKey[$06] + nKey[$02]), $12);

    nKey[$03] := nKey[$03] xor Rol32((nKey[$0F] + nKey[$0B]), $07);
    nKey[$07] := nKey[$07] xor Rol32((nKey[$03] + nKey[$0F]), $09);
    nKey[$0B] := nKey[$0B] xor Rol32((nKey[$07] + nKey[$03]), $0D);
    nKey[$0F] := nKey[$0F] xor Rol32((nKey[$0B] + nKey[$07]), $12);

    nKey[$01] := nKey[$01] xor Rol32((nKey[$00] + nKey[$03]), $07);
    nKey[$02] := nKey[$02] xor Rol32((nKey[$01] + nKey[$00]), $09);
    nKey[$03] := nKey[$03] xor Rol32((nKey[$02] + nKey[$01]), $0D);
    nKey[$00] := nKey[$00] xor Rol32((nKey[$03] + nKey[$02]), $12);

    nKey[$06] := nKey[$06] xor Rol32((nKey[$05] + nKey[$04]), $07);
    nKey[$07] := nKey[$07] xor Rol32((nKey[$06] + nKey[$05]), $09);
    nKey[$04] := nKey[$04] xor Rol32((nKey[$07] + nKey[$06]), $0D);
    nKey[$05] := nKey[$05] xor Rol32((nKey[$04] + nKey[$07]), $12);

    nKey[$0B] := nKey[$0B] xor Rol32((nKey[$0A] + nKey[$09]), $07);
    nKey[$08] := nKey[$08] xor Rol32((nKey[$0B] + nKey[$0A]), $09);
    nKey[$09] := nKey[$09] xor Rol32((nKey[$08] + nKey[$0B]), $0D);
    nKey[$0A] := nKey[$0A] xor Rol32((nKey[$09] + nKey[$08]), $12);

    nKey[$0C] := nKey[$0C] xor Rol32((nKey[$0F] + nKey[$0E]), $07);
    nKey[$0D] := nKey[$0D] xor Rol32((nKey[$0C] + nKey[$0F]), $09);
    nKey[$0E] := nKey[$0E] xor Rol32((nKey[$0D] + nKey[$0C]), $0D);
    nKey[$0F] := nKey[$0F] xor Rol32((nKey[$0E] + nKey[$0D]), $12);
  end;

  for i := 0 to 15 do
    Inc(nKey[i], Key[i]);

  for i := 0 to Len div 4 - 1 do
    D4[i]:= S4[i] xor nKey[i];

  for i := Len - Len mod 4 to Len - 1 do
    D1[i]:= S1[i] xor nKey1[i];
end;

procedure TSalsa20Data.Initialize(AKey: array of Byte;
  Vector: array of LongWord; ARounds: Integer);
var usedKey: PAnsiChar;
    ofs: Integer;
begin
  if Length(AKey) = 16 then begin
    usedKey:= key16;
    ofs:= 0;
  end else begin
    usedKey:= key32;
    ofs:= $10;
  end;
  Key[$0]:= PLongWord(@usedKey[$0])^;
  Key[$1]:= PLongWord(@AKey[$0])^;
  Key[$2]:= PLongWord(@AKey[$4])^;
  Key[$3]:= PLongWord(@AKey[$8])^;
  Key[$4]:= PLongWord(@AKey[$C])^;
  Key[$5]:= PLongWord(@usedKey[$4])^;
  Key[$6]:= Vector[$0];
  Key[$7]:= Vector[$1];
  Key[$8]:= Vector[$2];
  Key[$9]:= Vector[$3];
  Key[$A]:= PLongWord(@usedKey[$8])^;
  Key[$B]:= PLongWord(@AKey[$0 + ofs])^;
  Key[$C]:= PLongWord(@AKey[$4 + ofs])^;
  Key[$D]:= PLongWord(@AKey[$8 + ofs])^;
  Key[$E]:= PLongWord(@AKey[$C + ofs])^;
  Key[$F]:= PLongWord(@usedKey[$C])^;

  Rounds:= ARounds;
end;

{ TMD5HashAlg }

procedure TMD5HashAlg.AppendBuffer(const Buffer: TMD5Block);
var Store: TMD5Hash;
begin
  Store:= FHash;
  with FHash do begin
    { Round 1 }
    { Note:
        (Buff and y) or ( (not Buff) and z)
      is equivalent to
        (((z xor y) and Buff) xor z)
      -HHellström }
    A := ROL(A + (((D xor C) and B) xor D) + Buffer[ 0] + MD5_SINE[ 1],  7) + B;//
    D := ROL(D + (((C xor B) and A) xor C) + Buffer[ 1] + MD5_SINE[ 2], 12) + A;//
    C := ROL(C + (((B xor A) and D) xor B) + Buffer[ 2] + MD5_SINE[ 3], 17) + D;//
    B := ROL(B + (((A xor D) and C) xor A) + Buffer[ 3] + MD5_SINE[ 4], 22) + C;//
    A := ROL(A + (((D xor C) and B) xor D) + Buffer[ 4] + MD5_SINE[ 5],  7) + B;//
    D := ROL(D + (((C xor B) and A) xor C) + Buffer[ 5] + MD5_SINE[ 6], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buffer[ 6] + MD5_SINE[ 7], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buffer[ 7] + MD5_SINE[ 8], 22) + C;
    A := ROL(A + (((D xor C) and B) xor D) + Buffer[ 8] + MD5_SINE[ 9],  7) + B;
    D := ROL(D + (((C xor B) and A) xor C) + Buffer[ 9] + MD5_SINE[10], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buffer[10] + MD5_SINE[11], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buffer[11] + MD5_SINE[12], 22) + C;
    A := ROL(A + (((D xor C) and B) xor D) + Buffer[12] + MD5_SINE[13],  7) + B;
    D := ROL(D + (((C xor B) and A) xor C) + Buffer[13] + MD5_SINE[14], 12) + A;
    C := ROL(C + (((B xor A) and D) xor B) + Buffer[14] + MD5_SINE[15], 17) + D;
    B := ROL(B + (((A xor D) and C) xor A) + Buffer[15] + MD5_SINE[16], 22) + C;

    { Round 2 }
    { Note:
        (Buff and z) or (y and (not z) )
      is equivalent to
        (((y xor Buff) and z) xor y)
      -HHellström }
    A := ROL(A + (C xor (D and (B xor C))) + Buffer[ 1] + MD5_SINE[17],  5) + B;//
    D := ROL(D + (B xor (C and (A xor B))) + Buffer[ 6] + MD5_SINE[18],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buffer[11] + MD5_SINE[19], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buffer[ 0] + MD5_SINE[20], 20) + C;//
    A := ROL(A + (C xor (D and (B xor C))) + Buffer[ 5] + MD5_SINE[21],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buffer[10] + MD5_SINE[22],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buffer[15] + MD5_SINE[23], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buffer[ 4] + MD5_SINE[24], 20) + C;//
    A := ROL(A + (C xor (D and (B xor C))) + Buffer[ 9] + MD5_SINE[25],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buffer[14] + MD5_SINE[26],  9) + A;
    C := ROL(C + (A xor (B and (D xor A))) + Buffer[ 3] + MD5_SINE[27], 14) + D;//
    B := ROL(B + (D xor (A and (C xor D))) + Buffer[ 8] + MD5_SINE[28], 20) + C;
    A := ROL(A + (C xor (D and (B xor C))) + Buffer[13] + MD5_SINE[29],  5) + B;
    D := ROL(D + (B xor (C and (A xor B))) + Buffer[ 2] + MD5_SINE[30],  9) + A;//
    C := ROL(C + (A xor (B and (D xor A))) + Buffer[ 7] + MD5_SINE[31], 14) + D;
    B := ROL(B + (D xor (A and (C xor D))) + Buffer[12] + MD5_SINE[32], 20) + C;

    { Round 3. }
    A := ROL(A + (B xor C xor D) + Buffer[ 5] + MD5_SINE[33],  4) + B;
    D := ROL(D + (A xor B xor C) + Buffer[ 8] + MD5_SINE[34], 11) + A;
    C := ROL(C + (D xor A xor B) + Buffer[11] + MD5_SINE[35], 16) + D;
    B := ROL(B + (C xor D xor A) + Buffer[14] + MD5_SINE[36], 23) + C;
    A := ROL(A + (B xor C xor D) + Buffer[ 1] + MD5_SINE[37],  4) + B;//
    D := ROL(D + (A xor B xor C) + Buffer[ 4] + MD5_SINE[38], 11) + A;//
    C := ROL(C + (D xor A xor B) + Buffer[ 7] + MD5_SINE[39], 16) + D;
    B := ROL(B + (C xor D xor A) + Buffer[10] + MD5_SINE[40], 23) + C;
    A := ROL(A + (B xor C xor D) + Buffer[13] + MD5_SINE[41],  4) + B;
    D := ROL(D + (A xor B xor C) + Buffer[ 0] + MD5_SINE[42], 11) + A;//
    C := ROL(C + (D xor A xor B) + Buffer[ 3] + MD5_SINE[43], 16) + D;//
    B := ROL(B + (C xor D xor A) + Buffer[ 6] + MD5_SINE[44], 23) + C;
    A := ROL(A + (B xor C xor D) + Buffer[ 9] + MD5_SINE[45],  4) + B;
    D := ROL(D + (A xor B xor C) + Buffer[12] + MD5_SINE[46], 11) + A;
    C := ROL(C + (D xor A xor B) + Buffer[15] + MD5_SINE[47], 16) + D;
    B := ROL(B + (C xor D xor A) + Buffer[ 2] + MD5_SINE[48], 23) + C;//

    { Round 4. }
    A := ROL(A + ((B or not D) xor C) + Buffer[ 0] + MD5_SINE[49],  6) + B;//
    D := ROL(D + ((A or not C) xor B) + Buffer[ 7] + MD5_SINE[50], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buffer[14] + MD5_SINE[51], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buffer[ 5] + MD5_SINE[52], 21) + C;
    A := ROL(A + ((B or not D) xor C) + Buffer[12] + MD5_SINE[53],  6) + B;
    D := ROL(D + ((A or not C) xor B) + Buffer[ 3] + MD5_SINE[54], 10) + A;//
    C := ROL(C + ((D or not B) xor A) + Buffer[10] + MD5_SINE[55], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buffer[ 1] + MD5_SINE[56], 21) + C;//
    A := ROL(A + ((B or not D) xor C) + Buffer[ 8] + MD5_SINE[57],  6) + B;
    D := ROL(D + ((A or not C) xor B) + Buffer[15] + MD5_SINE[58], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buffer[ 6] + MD5_SINE[59], 15) + D;
    B := ROL(B + ((C or not A) xor D) + Buffer[13] + MD5_SINE[60], 21) + C;
    A := ROL(A + ((B or not D) xor C) + Buffer[ 4] + MD5_SINE[61],  6) + B;//
    D := ROL(D + ((A or not C) xor B) + Buffer[11] + MD5_SINE[62], 10) + A;
    C := ROL(C + ((D or not B) xor A) + Buffer[ 2] + MD5_SINE[63], 15) + D;//
    B := ROL(B + ((C or not A) xor D) + Buffer[ 9] + MD5_SINE[64], 21) + C;

    Inc(A, Store.A);
    Inc(B, Store.B);
    Inc(C, Store.C);
    Inc(D, Store.D);
  end;
  Inc(FBitLength, SizeOf(Buffer) * 8);
end;

function TMD5HashAlg.Finalize(const Buffer; Length: Integer): TMD5Hash;
var
  i: Integer;
  x: array [0..0] of Byte absolute Buffer;
  lastBlock: TMD5Block;
  byteBlock: array [0..63] of Byte absolute lastBlock;
begin
  for i := 0 to Length div SizeOf(TMD5Block) - 1 do begin
    Move(x[i * SizeOf(TMD5Block)], lastBlock, SizeOf(TMD5Block));
    AppendBuffer(lastBlock);
  end;
  i:= Length mod SizeOf(lastBlock);
  FillChar(lastBlock, SizeOf(lastBlock), 0);
  Move(x[Length - i], lastBlock, i);
  byteBlock[i]:= $80;
  if i + 9 > SizeOf(lastBlock) then begin
    AppendBuffer(lastBlock);
    FillChar(lastBlock, SizeOf(lastBlock), 0);
    Dec(FBitLength, SizeOf(lastBlock) * 8);
  end;
  Inc(FBitLength, i * 8);
  Move(FBitLength, lastBlock[14], 4);
  AppendBuffer(lastBlock);
  Result:= FHash;
end;

function TMD5HashAlg.GetStreamHash(AStream: TStream): TMD5Hash;
var
  readed: Integer;
  lastBlock: TMD5Block;
  byteBlock: array [0..63] of Byte absolute lastBlock;
begin
  Initialize;
  readed:= AStream.Read(lastBlock, SizeOf(lastBlock));
  while readed = SizeOf(lastBlock) do begin
    AppendBuffer(lastBlock);
    readed:= AStream.Read(lastBlock, SizeOf(lastBlock));
  end;
  Result:= Finalize(lastBlock, readed);
end;

procedure TMD5HashAlg.Initialize(const Start: TMD5Hash);
begin
  FHash:= Start;
  FBitLength:= 0;
end;

procedure TMD5HashAlg.Initialize;
begin
  with FHash do begin
    A := MD4_INIT_VALUES[0];
    B := MD4_INIT_VALUES[1];
    C := MD4_INIT_VALUES[2];
    D := MD4_INIT_VALUES[3];
  end;
  FBitLength:= 0;
end;

end.
