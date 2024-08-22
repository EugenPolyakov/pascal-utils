{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit System.Hash;

interface

uses
  System.SysUtils;

type
  /// <summary> Hash related Exceptions </summary>
  EHashException = class(Exception);

  /// <summary> Record with common functionality to all Hash functions</summary>
  THash = record
    /// <summary>Convert a Digest into an Integer if it's length its four</summary>
    class function DigestAsInteger(const ADigest: TBytes): Integer; static;
    /// <summary>Convert a Digest into a hexadecimal value string</summary>
    class function DigestAsString(const ADigest: TBytes): string; static;
    /// <summary>Convert a Digest into a GUID if it's length its sixteen</summary>
    class function DigestAsStringGUID(const ADigest: TBytes): string; static;
    /// <summary> Gets a random string with the given length</summary>
    class function GetRandomString(const ALen: Integer = 10): string; static;
    /// <summary> Gets the BigEndian memory representation of a cardinal value</summary>
    class function ToBigEndian(AValue: Cardinal): Cardinal; static; inline;
  end;


  /// <summary> Record to generate MD5 Hash values from data. Stores internal state of the process</summary>
  THashMD5 = record
  private type
    TContextState = array [0..15] of Cardinal;
    TContextBuffer = array [0..63] of Byte;
  private
    FPadding: TContextBuffer;
    FContextState: array [0..3] of Cardinal;
    FContextCount: array [0..1] of Cardinal;
    FContextBuffer: TContextBuffer;
    FFinalized: Boolean;
    procedure Transform(const ABlock: PByte; AShift: Integer);
    procedure Decode(const Dst: PCardinal; const Src: PByte; Len: Integer; AShift: Integer);
    procedure Encode(const Dst: PByte; const Src: PCardinal; Len: Integer);
    procedure FinalizeHash;
    procedure Update(const AData: PByte; ALength: Cardinal); overload;

    function GetDigest: TBytes;
  public

    /// <summary> Creates an instance to generate MD5 hashes</summary>
    class function Create: THashMD5; static;
    /// <summary> Puts the state machine of the generator in it's initial state.</summary>
    procedure Reset;
    /// <summary> Update the Hash with the provided bytes</summary>
    procedure Update(const AData; ALength: Cardinal); overload;
    procedure Update(const AData: TBytes; ALength: Cardinal = 0); overload;
    procedure Update(const Input: string); overload;

    /// <summary> Returns the hash value as a TBytes</summary>
    function HashAsBytes: TBytes;
    /// <summary> Returns the hash value as string</summary>
    function HashAsString: string;

    /// <summary> Hash the given string and returns it's hash value as integer</summary>
    class function GetHashBytes(const AData: string): TBytes; static;
    /// <summary> Hash the given string and returns it's hash value as string</summary>
    class function GetHashString(const AString: string): string; static;

    /// <summary>Gets the string associated to the HMAC authentication</summary>
    class function GetHMAC(const AData, AKey: string): string; static; inline;
    /// <summary>Gets the Digest associated to the HMAC authentication</summary>
    class function GetHMACAsBytes(const AData, AKey: string): TBytes; static;
  end;

  /// <summary> Record to generate SHA1 Hash values from data</summary>
  THashSHA1 = record
  public type
    TDigest = array[0..19] of Byte;
  private
    FHash: array[0..4] of Cardinal;
    FBitLength: Int64;
    FBuffer: array [0..63] of Byte;
    FIndex: Integer;
    FFinalized: Boolean;
    procedure Initialize;
    procedure CheckFinalized;
    procedure Compress;
    procedure Finalize;

    function GetDigest: TBytes;
  public
    /// <summary>Initialize the Record used to calculate the SHA1 Hash</summary>
    class function Create: THashSHA1; static;

    /// <summary> Puts the state machine of the generator in it's initial state.</summary>
    procedure Reset;

    /// <summary> Update the Hash value with the given Data. </summary>
    procedure Update(const AData; ALength: Cardinal); overload;
    procedure Update(const AData: TBytes; ALength: Cardinal = 0); overload;
    procedure Update(const Input: string); overload;

    /// <summary> Returns the hash value as a TBytes</summary>
    function HashAsBytes: TBytes;
    /// <summary> Returns the hash value as string</summary>
    function HashAsString: string;

    /// <summary> Hash the given string and returns it's hash value as integer</summary>
    class function GetHashBytes(const AData: string): TBytes; static;
    /// <summary> Hash the given string and returns it's hash value as string</summary>
    class function GetHashString(const AString: string): string; static;

    /// <summary>Gets the string associated to the HMAC authentication</summary>
    class function GetHMAC(const AData, AKey: string): string; overload; static; inline;
    /// <summary>Gets the Digest associated to the HMAC authentication</summary>
    class function GetHMACAsBytes(const AData, AKey: string): TBytes; overload; static;

    /// <summary>Gets the string associated to the HMAC authentication</summary>
    class function GetHMAC(const AData, AKey: TBytes): string; overload; static; inline;
    /// <summary>Gets the Digest associated to the HMAC authentication</summary>
    class function GetHMACAsBytes(const AData, AKey: TBytes): TBytes; overload; static;
  end;

  /// <summary> Record to generate BobJenkins Hash values from data. Stores internal state of the process</summary>
  THashBobJenkins = record
  private
    FHash: Integer;
    function GetDigest: TBytes;

    class function HashLittle(const Data; Len, InitVal: Integer): Integer; static;
  public
    /// <summary>Initialize the Record used to calculate the BobJenkins Hash</summary>
    class function Create: THashBobJenkins; static;

    /// <summary> Puts the state machine of the generator in it's initial state.</summary>
    procedure Reset;

    /// <summary> Update the Hash value with the given Data. </summary>
    procedure Update(const AData; ALength: Cardinal); overload;
    procedure Update(const AData: TBytes; ALength: Cardinal = 0); overload;
    procedure Update(const Input: string); overload;

    /// <summary> Returns the hash value as a TBytes</summary>
    function HashAsBytes: TBytes;
    /// <summary> Returns the hash value as integer</summary>
    function HashAsInteger: Integer;
    /// <summary> Returns the hash value as string</summary>
    function HashAsString: string;

    /// <summary> Hash the given string and returns it's hash value as integer</summary>
    class function GetHashBytes(const AData: string): TBytes; static;
    /// <summary> Hash the given string and returns it's hash value as string</summary>
    class function GetHashString(const AString: string): string; static;
    /// <summary> Hash the given string and returns it's hash value as integer</summary>
    class function GetHashValue(const AData: string): Integer; overload; static; inline;
    /// <summary> Hash the given Data and returns it's hash value as integer</summary>
    class function GetHashValue(const AData; ALength: Integer; AInitialValue: Integer = 0): Integer; overload; static; inline;
  end;

resourcestring
  SHashCanNotUpdateMD5 = 'Can''t update MD5 after finalization';
  SHashCanNotUpdateSHA1 = 'Can''t update SHA1 after finalization';

implementation

uses
  System.Types;

{$OVERFLOWCHECKS OFF}

{ THashMD5 }

class function THashMD5.Create: THashMD5;
begin
  Result.Reset;
end;

procedure THashMD5.Reset;
begin
  FillChar(FPadding, 64, 0);
  FPadding[0] := $80;
  FContextCount[0] := 0;
  FContextCount[1] := 0;
  FContextState[0] := $67452301;
  FContextState[1] := $efcdab89;
  FContextState[2] := $98badcfe;
  FContextState[3] := $10325476;
  FillChar(FContextBuffer, 64, 0);
  FFinalized := False;
end;

procedure THashMD5.Transform(const ABlock: PByte; AShift: Integer);

  function F(x, y, z: Cardinal): Cardinal; inline;
  begin
    Result := (x and y) or ((not x) and z);
  end;

  function G(x, y, z: Cardinal): Cardinal; inline;
  begin
    Result := (x and z) or (y and (not z));
  end;

  function H(x, y, z: Cardinal): Cardinal; inline;
  begin
    Result := x xor y xor z;
  end;

  function I(x, y, z: Cardinal): Cardinal; inline;
  begin
    Result := y xor (x or (not z));
  end;

  procedure RL(var x: Cardinal; n: Byte); inline;
  begin
    x := (x shl n) or (x shr (32 - n));
  end;

  procedure FF(var a: Cardinal; b, c, d, x: Cardinal; s: Byte; ac: Cardinal);
  begin
    Inc(a, F(b, c, d) + x + ac);
    RL(a, s);
    Inc(a, b);
  end;

  procedure GG(var a: Cardinal; b, c, d, x: Cardinal; s: Byte; ac: Cardinal);
  begin
    Inc(a, G(b, c, d) + x + ac);
    RL(a, s);
    Inc(a, b);
  end;

  procedure HH(var a: Cardinal; b, c, d, x: Cardinal; s: Byte; ac: Cardinal);
  begin
    Inc(a, H(b, c, d) + x + ac);
    RL(a, s);
    Inc(a, b);
  end;

  procedure II(var a: Cardinal; b, c, d, x: Cardinal; s: Byte; ac: Cardinal);
  begin
    Inc(a, I(b, c, d) + x + ac);
    RL(a, s);
    Inc(a, b);
  end;

const
  S11 =  7;
  S12 = 12;
  S13 = 17;
  S14 = 22;

  S21 =  5;
  S22 =  9;
  S23 = 14;
  S24 = 20;

  S31 =  4;
  S32 = 11;
  S33 = 16;
  S34 = 23;

  S41 =  6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
  a, b, c, d: Cardinal;
  x: TContextState;
begin
  a := FContextState[0];
  b := FContextState[1];
  c := FContextState[2];
  d := FContextState[3];

  Decode(PCardinal(@x[0]), ABlock, 64, AShift);

  { Round 1 }
  FF( a, b, c, d, x[ 0], S11, $d76aa478); { 1 }
  FF( d, a, b, c, x[ 1], S12, $e8c7b756); { 2 }
  FF( c, d, a, b, x[ 2], S13, $242070db); { 3 }
  FF( b, c, d, a, x[ 3], S14, $c1bdceee); { 4 }
  FF( a, b, c, d, x[ 4], S11, $f57c0faf); { 5 }
  FF( d, a, b, c, x[ 5], S12, $4787c62a); { 6 }
  FF( c, d, a, b, x[ 6], S13, $a8304613); { 7 }
  FF( b, c, d, a, x[ 7], S14, $fd469501); { 8 }
  FF( a, b, c, d, x[ 8], S11, $698098d8); { 9 }
  FF( d, a, b, c, x[ 9], S12, $8b44f7af); { 10 }
  FF( c, d, a, b, x[10], S13, $ffff5bb1); { 11 }
  FF( b, c, d, a, x[11], S14, $895cd7be); { 12 }
  FF( a, b, c, d, x[12], S11, $6b901122); { 13 }
  FF( d, a, b, c, x[13], S12, $fd987193); { 14 }
  FF( c, d, a, b, x[14], S13, $a679438e); { 15 }
  FF( b, c, d, a, x[15], S14, $49b40821); { 16 }

  { Round 2 }
  GG( a, b, c, d, x[ 1], S21, $f61e2562); { 17 }
  GG( d, a, b, c, x[ 6], S22, $c040b340); { 18 }
  GG( c, d, a, b, x[11], S23, $265e5a51); { 19 }
  GG( b, c, d, a, x[ 0], S24, $e9b6c7aa); { 20 }
  GG( a, b, c, d, x[ 5], S21, $d62f105d); { 21 }
  GG( d, a, b, c, x[10], S22,  $2441453); { 22 }
  GG( c, d, a, b, x[15], S23, $d8a1e681); { 23 }
  GG( b, c, d, a, x[ 4], S24, $e7d3fbc8); { 24 }
  GG( a, b, c, d, x[ 9], S21, $21e1cde6); { 25 }
  GG( d, a, b, c, x[14], S22, $c33707d6); { 26 }
  GG( c, d, a, b, x[ 3], S23, $f4d50d87); { 27 }
  GG( b, c, d, a, x[ 8], S24, $455a14ed); { 28 }
  GG( a, b, c, d, x[13], S21, $a9e3e905); { 29 }
  GG( d, a, b, c, x[ 2], S22, $fcefa3f8); { 30 }
  GG( c, d, a, b, x[ 7], S23, $676f02d9); { 31 }
  GG( b, c, d, a, x[12], S24, $8d2a4c8a); { 32 }

  { Round 3 }
  HH( a, b, c, d, x[ 5], S31, $fffa3942); { 33 }
  HH( d, a, b, c, x[ 8], S32, $8771f681); { 34 }
  HH( c, d, a, b, x[11], S33, $6d9d6122); { 35 }
  HH( b, c, d, a, x[14], S34, $fde5380c); { 36 }
  HH( a, b, c, d, x[ 1], S31, $a4beea44); { 37 }
  HH( d, a, b, c, x[ 4], S32, $4bdecfa9); { 38 }
  HH( c, d, a, b, x[ 7], S33, $f6bb4b60); { 39 }
  HH( b, c, d, a, x[10], S34, $bebfbc70); { 40 }
  HH( a, b, c, d, x[13], S31, $289b7ec6); { 41 }
  HH( d, a, b, c, x[ 0], S32, $eaa127fa); { 42 }
  HH( c, d, a, b, x[ 3], S33, $d4ef3085); { 43 }
  HH( b, c, d, a, x[ 6], S34,  $4881d05); { 44 }
  HH( a, b, c, d, x[ 9], S31, $d9d4d039); { 45 }
  HH( d, a, b, c, x[12], S32, $e6db99e5); { 46 }
  HH( c, d, a, b, x[15], S33, $1fa27cf8); { 47 }
  HH( b, c, d, a, x[ 2], S34, $c4ac5665); { 48 }

  { Round 4 }
  II( a, b, c, d, x[ 0], S41, $f4292244); { 49 }
  II( d, a, b, c, x[ 7], S42, $432aff97); { 50 }
  II( c, d, a, b, x[14], S43, $ab9423a7); { 51 }
  II( b, c, d, a, x[ 5], S44, $fc93a039); { 52 }
  II( a, b, c, d, x[12], S41, $655b59c3); { 53 }
  II( d, a, b, c, x[ 3], S42, $8f0ccc92); { 54 }
  II( c, d, a, b, x[10], S43, $ffeff47d); { 55 }
  II( b, c, d, a, x[ 1], S44, $85845dd1); { 56 }
  II( a, b, c, d, x[ 8], S41, $6fa87e4f); { 57 }
  II( d, a, b, c, x[15], S42, $fe2ce6e0); { 58 }
  II( c, d, a, b, x[ 6], S43, $a3014314); { 59 }
  II( b, c, d, a, x[13], S44, $4e0811a1); { 60 }
  II( a, b, c, d, x[ 4], S41, $f7537e82); { 61 }
  II( d, a, b, c, x[11], S42, $bd3af235); { 62 }
  II( c, d, a, b, x[ 2], S43, $2ad7d2bb); { 63 }
  II( b, c, d, a, x[ 9], S44, $eb86d391); { 64 }

  Inc(FContextState[0], a);
  Inc(FContextState[1], b);
  Inc(FContextState[2], c);
  Inc(FContextState[3], d);
end;

procedure THashMD5.Update(const AData; ALength: Cardinal);
begin
  Update(PByte(@AData), ALength);
end;

procedure THashMD5.Update(const AData: TBytes; ALength: Cardinal = 0);
var
  LLen: Cardinal;
begin
  LLen := ALength;
  if LLen = 0 then
    LLen := Length(AData);
  Update(PByte(AData)^, LLen);
end;

procedure THashMD5.Update(const AData: PByte; ALength: Cardinal);
var
  Index, PartLen, I, Start: Cardinal;
begin
  if FFinalized then
    raise EHashException.CreateRes(@SHashCanNotUpdateMD5);

  { Compute number of bytes mod 64 }
  Index := (FContextCount[0] shr 3) and $3f;
  { Update number of bits }
  Inc(FContextCount[0], ALength shl 3);
  if FContextCount[0] < (ALength shl 3) then
    Inc(FContextCount[1]);
  Inc(FContextCount[1], ALength shr 29);
  PartLen := 64 - Index;

  { Transform (as many times as possible) }
  if ALength >= PartLen then
  begin
    for I := 0 to PartLen - 1 do
      FContextBuffer[I + Index] := AData[I];

    Transform(PByte(@FContextBuffer[0]), 0);
    I := PartLen;
    while (I + 63) < ALength do
    begin
      Transform(AData, I);
      Inc(I, 64);
    end;
    Index := 0;
  end
  else
    I := 0;

  { Input remaining input }
  if I < ALength then
  begin
    Start := I;
    while I < ALength do
    begin
      FContextBuffer[Index + I - Start] := AData[I];
      Inc(I);
    end;
  end;
end;

procedure THashMD5.Update(const Input: string);
begin
  Update(TEncoding.UTF8.GetBytes(Input));
end;

{$POINTERMATH ON}
procedure THashMD5.Encode(const Dst: PByte; const Src: PCardinal; Len: Integer);
var
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while J < Len do
  begin
    Dst[J]   := Byte((Src[I]       ) and $ff);
    Dst[J+1] := Byte((Src[I] shr  8) and $ff);
    Dst[J+2] := Byte((Src[I] shr 16) and $ff);
    Dst[J+3] := Byte((Src[I] shr 24) and $ff);
    Inc(J, 4);
    Inc(I);
  end;
end;

function THashMD5.GetDigest: TBytes;
begin
  if not FFinalized then
    FinalizeHash;
  { Store state in digest }
  SetLength(Result, 16);
  Encode(PByte(Result), PCardinal(@FContextState[0]), 16);
end;

class function THashMD5.GetHashBytes(const AData: string): TBytes;
var
  LMD5: THashMD5;
begin
  LMD5 := THashMD5.Create;
  LMD5.Update(AData);
  Result := LMD5.GetDigest;
end;

class function THashMD5.GetHashString(const AString: string): string;
var
  LMD5: THashMD5;
begin
  LMD5 := THashMD5.Create;
  LMD5.Update(AString);
  Result := LMD5.HashAsString;
end;

class function THashMD5.GetHMAC(const AData, AKey: string): string;
begin
  Result := THash.DigestAsString(GetHMACAsBytes(AData, AKey));
end;

class function THashMD5.GetHMACAsBytes(const AData, AKey: string): TBytes;
const
  CInnerPad : Byte = $36;
  COuterPad : Byte = $5C;
  FHashSize : Integer = 16;
  FBlockSize: Integer = 64;
var
  TempBuffer1: TBytes;
  TempBuffer2: TBytes;
  FKey: TBytes;
  LKey: TBytes;
  I: Integer;
  FHash: THashMD5;
  LBuffer: TBytes;
begin
  FHash := THashMD5.Create;

  LBuffer := TEncoding.UTF8.GetBytes(AData);

  FKey := TEncoding.UTF8.GetBytes(AKey);
  if Length(FKey) > FBlockSize then
  begin
    FHash.Update(FKey);
    FKey := Copy(FHash.GetDigest);
  end;

  LKey := Copy(FKey, 0, MaxInt);
  SetLength(LKey, FBlockSize);
  SetLength(TempBuffer1, FBlockSize + Length(LBuffer));
  for I := Low(LKey) to High(LKey) do begin
    TempBuffer1[I] := LKey[I] xor CInnerPad;
  end;
  if Length(LBuffer) > 0 then
    Move(LBuffer[0], TempBuffer1[Length(LKey)], Length(LBuffer));

  FHash.Reset;
  FHash.Update(TempBuffer1);
  TempBuffer2 := FHash.GetDigest;

  SetLength(TempBuffer1, FBlockSize + FHashSize);
  for I := Low(LKey) to High(LKey) do begin
    TempBuffer1[I] := LKey[I] xor COuterPad;
  end;
  Move(TempBuffer2[0], TempBuffer1[Length(LKey)], Length(TempBuffer2));

  FHash.Reset;
  FHash.Update(TempBuffer1);
  Result := FHash.GetDigest;
end;

function THashMD5.HashAsBytes: TBytes;
begin
  Result := GetDigest;
end;

function THashMD5.HashAsString: string;
begin
  Result := THash.DigestAsString(GetDigest);
end;

procedure THashMD5.FinalizeHash;
var
  Bits: TContextBuffer;
  Index: Integer;
  PadLen: Cardinal;
begin
  { Save number of bits }
  Encode(PByte(@Bits[0]), PCardinal(@FContextCount[0]), 8);
  { Pad out to 56 mod 64 }
  Index := (FContextCount[0] shr 3) and $3F;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  Update(PByte(@FPadding[0]), PadLen);
  { Append length (before padding) }
  Update(PByte(@Bits[0]), 8);
  FFinalized := True;
end;

procedure THashMD5.Decode(const Dst: PCardinal; const Src: PByte; Len: Integer; AShift: Integer);
var
  I, J: Integer;
  a, b, c, d: Byte;
begin
  J := 0;
  I := 0;
  while (J < Len) do
  begin
    a := Src[J+AShift];
    b := Src[J+AShift+1];
    c := Src[J+AShift+2];
    d := Src[J+AShift+3];
    Dst[I] := Cardinal(a and $ff)         or
             (Cardinal(b and $ff) shl 8)  or
             (Cardinal(c and $ff) shl 16) or
             (Cardinal(d and $ff) shl 24);
    Inc(J, 4);
    Inc(I);
  end;
end;

{$POINTERMATH OFF}

{ THashSHA1 }

procedure THashSHA1.CheckFinalized;
begin
  if FFinalized then
    raise EHashException.CreateRes(@SHashCanNotUpdateSHA1);
end;

procedure THashSHA1.Update(const Input: string);
begin
  Update(TEncoding.UTF8.GetBytes(Input));
end;

procedure THashSHA1.Compress;

  function F1(X, Y, Z: Cardinal): Cardinal; inline;
  begin
    Result := (X and Y) or ((not X) and Z);
  end;

  function F2(X, Y, Z: Cardinal): Cardinal; inline;
  begin
    Result := X xor Y xor Z;
  end;

  function F3(X, Y, Z: Cardinal): Cardinal; inline;
  begin
    Result := (X and Y) or (X and Z) or (Y and Z);
  end;

  function LeftRotate(Value: Cardinal; N: Integer): Cardinal; inline;
  begin
    Result := (Value shl N) or (Value shr (32 - N));
  end;

var
  A, B, C, D, E, T: Cardinal;
  W: array[0..79] of Cardinal;
  I: Integer;
begin
  Move(FBuffer, W, Sizeof(FBuffer));
  for I := 0 to 15 do
    W[I] := THash.ToBigEndian(W[I]);
  for I := 16 to 79 do
    W[I] := LeftRotate(W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16], 1);
  A := FHash[0]; B := FHash[1]; C := FHash[2]; D := FHash[3]; E := FHash[4];
  for I := 0 to 19 do
  begin
    T := LeftRotate(A, 5) + F1(B, C, D) + E + W[I] + $5A827999;
    E := D; D := C; C := LeftRotate(B, 30); B := A; A := T;
  end;
  for I := 20 to 39 do
  begin
    T := LeftRotate(A, 5) + F2(B, C, D) + E + W[I] + $6ED9EBA1;
    E := D; D := C; C := LeftRotate(B, 30); B := A; A := T;
  end;
  for I := 40 to 59 do
  begin
    T := LeftRotate(A, 5) + F3(B, C, D) + E + W[I] + $8F1BBCDC;
    E := D; D:= C; C := LeftRotate(B, 30); B := A; A := T;
  end;
  for I := 60 to 79 do
  begin
    T := LeftRotate(A, 5) + F2(B, C, D) + E + W[I] + $CA62C1D6;
    E := D; D := C; C := LeftRotate(B, 30); B := A; A := T;
  end;
  FHash[0] := FHash[0] + A;
  FHash[1] := FHash[1] + B;
  FHash[2] := FHash[2] + C;
  FHash[3] := FHash[3] + D;
  FHash[4] := FHash[4] + E;
  FillChar(FBuffer, Sizeof(FBuffer), 0);
end;

class function THashSHA1.Create: THashSHA1;
begin
  Result.Reset;
end;

class function THashSHA1.GetHashBytes(const AData: string): TBytes;
var
  LSHA1: THashSHA1;
begin
  LSHA1 := THashSHA1.Create;
  LSHA1.Update(AData);
  Result := LSHA1.GetDigest;
end;

class function THashSHA1.GetHashString(const AString: string): string;
var
  LSHA1: THashSHA1;
begin
  LSHA1 := THashSHA1.Create;
  LSHA1.Update(AString);
  Result := LSHA1.HashAsString;
end;


class function THashSHA1.GetHMAC(const AData, AKey: TBytes): string;
begin
  Result := THash.DigestAsString(GetHMACAsBytes(AData, AKey));
end;

class function THashSHA1.GetHMAC(const AData, AKey: string): string;
begin
  Result := THash.DigestAsString(GetHMACAsBytes(AData, AKey));
end;

class function THashSHA1.GetHMACAsBytes(const AData, AKey: TBytes): TBytes;
const
  CInnerPad : Byte = $36;
  COuterPad : Byte = $5C;
  FHashSize : Integer = 20;
  FBlockSize: Integer = 64;
var
  TempBuffer1: TBytes;
  TempBuffer2: TBytes;
  FKey: TBytes;
  LKey: TBytes;
  I: Integer;
  FHash: THashSHA1;
  LBuffer: TBytes;
begin
  FHash := THashSHA1.Create;

  LBuffer := AData;

  FKey := AKey;
  if Length(FKey) > FBlockSize then
  begin
    FHash.Update(FKey);
    FKey := Copy(FHash.GetDigest);
  end;

  LKey := Copy(FKey, 0, MaxInt);
  SetLength(LKey, FBlockSize);
  SetLength(TempBuffer1, FBlockSize + Length(LBuffer));
  for I := Low(LKey) to High(LKey) do begin
    TempBuffer1[I] := LKey[I] xor CInnerPad;
  end;
  if Length(LBuffer) > 0 then
    Move(LBuffer[0], TempBuffer1[Length(LKey)], Length(LBuffer));

  FHash.Reset;
  FHash.Update(TempBuffer1);
  TempBuffer2 := FHash.GetDigest;

  SetLength(TempBuffer1, FBlockSize + FHashSize);
  for I := Low(LKey) to High(LKey) do begin
    TempBuffer1[I] := LKey[I] xor COuterPad;
  end;
  Move(TempBuffer2[0], TempBuffer1[Length(LKey)], Length(TempBuffer2));

  FHash.Reset;
  FHash.Update(TempBuffer1);
  Result := FHash.GetDigest;
end;

class function THashSHA1.GetHMACAsBytes(const AData, AKey: string): TBytes;
begin
  Result:= GetHMACAsBytes(TEncoding.UTF8.GetBytes(AData), TEncoding.UTF8.GetBytes(AKey));
end;

procedure THashSHA1.Finalize;
begin
  CheckFinalized;
  FBuffer[FIndex] := $80;
  if FIndex >= 56 then
    Compress;
  PCardinal(@FBuffer[56])^ := THash.ToBigEndian(Cardinal(FBitLength shr 32));
  PCardinal(@FBuffer[60])^ := THash.ToBigEndian(Cardinal(FBitLength));
  Compress;
  FHash[0] := THash.ToBigEndian(FHash[0]);
  FHash[1] := THash.ToBigEndian(FHash[1]);
  FHash[2] := THash.ToBigEndian(FHash[2]);
  FHash[3] := THash.ToBigEndian(FHash[3]);
  FHash[4] := THash.ToBigEndian(FHash[4]);
  FFinalized := True;
end;

function THashSHA1.GetDigest: TBytes;
begin
  if not FFinalized then
    Finalize;
  SetLength(Result, 20); // Size of Hash...
  Move(PByte(@FHash[0])^, PByte(@Result[0])^, Length(Result));
end;

procedure THashSHA1.Initialize;
begin
  FillChar(Self, SizeOf(Self), 0);
  FHash[0] := $67452301;
  FHash[1] := $EFCDAB89;
  FHash[2] := $98BADCFE;
  FHash[3] := $10325476;
  FHash[4] := $C3D2E1F0;
end;

procedure THashSHA1.Reset;
begin
  Initialize;
end;

procedure THashSHA1.Update(const AData; ALength: Cardinal);
var
  Buffer: PByte;
begin
  CheckFinalized;
  Buffer := PByte(@AData);

  Inc(FBitLength, ALength * 8);
  while ALength > 0 do
  begin
    FBuffer[FIndex] := Buffer[0];
    Inc(Buffer);
    Inc(FIndex);
    Dec(ALength);
    if FIndex = 64 then
    begin
      FIndex := 0;
      Compress;
    end;
  end;
end;

procedure THashSHA1.Update(const AData: TBytes; ALength: Cardinal = 0);
var
  Len: Integer;
begin
  if ALength = 0 then
    Len := Length(AData)
  else
    Len := ALength;
  Update(PByte(AData)^, Len);
end;

function THashSHA1.HashAsBytes: TBytes;
begin
  Result := GetDigest;
end;

function THashSHA1.HashAsString: string;
begin
  Result := THash.DigestAsString(GetDigest);
end;

{ THashBobJenkins }

class function THashBobJenkins.HashLittle(const Data; Len, InitVal: Integer): Integer;
  function Rot(x, k: Cardinal): Cardinal; inline;
  begin
    Result := (x shl k) or (x shr (32 - k));
  end;

  procedure Mix(var a, b, c: Cardinal); inline;
  begin
    Dec(a, c); a := a xor Rot(c, 4); Inc(c, b);
    Dec(b, a); b := b xor Rot(a, 6); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 8); Inc(b, a);
    Dec(a, c); a := a xor Rot(c,16); Inc(c, b);
    Dec(b, a); b := b xor Rot(a,19); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 4); Inc(b, a);
  end;

  procedure Final(var a, b, c: Cardinal); inline;
  begin
    c := c xor b; Dec(c, Rot(b,14));
    a := a xor c; Dec(a, Rot(c,11));
    b := b xor a; Dec(b, Rot(a,25));
    c := c xor b; Dec(c, Rot(b,16));
    a := a xor c; Dec(a, Rot(c, 4));
    b := b xor a; Dec(b, Rot(a,14));
    c := c xor b; Dec(c, Rot(b,24));
  end;

{$POINTERMATH ON}
var
  pb: PByte;
  pd: PCardinal absolute pb;
  a, b, c: Cardinal;
label
  case_1, case_2, case_3, case_4, case_5, case_6,
  case_7, case_8, case_9, case_10, case_11, case_12;
begin
  a := Cardinal($DEADBEEF) + Cardinal(Len shl 2) + Cardinal(InitVal);
  b := a;
  c := a;

  pb := @Data;

  // 4-byte aligned data
  if (Cardinal(pb) and 3) = 0 then
  begin
    while Len > 12 do
    begin
      Inc(a, pd[0]);
      Inc(b, pd[1]);
      Inc(c, pd[2]);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(pd, 3);
    end;

    case Len of
      0: Exit(Integer(c));
      1: Inc(a, pd[0] and $FF);
      2: Inc(a, pd[0] and $FFFF);
      3: Inc(a, pd[0] and $FFFFFF);
      4: Inc(a, pd[0]);
      5:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FF);
      end;
      6:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FFFF);
      end;
      7:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1] and $FFFFFF);
      end;
      8:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
      end;
      9:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FF);
      end;
      10:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FFFF);
      end;
      11:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2] and $FFFFFF);
      end;
      12:
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2]);
      end;
    end;
  end
  else
  begin
    // Ignoring rare case of 2-byte aligned data. This handles all other cases.
    while Len > 12 do
    begin
      Inc(a, pb[0] + pb[1] shl 8 + pb[2] shl 16 + pb[3] shl 24);
      Inc(b, pb[4] + pb[5] shl 8 + pb[6] shl 16 + pb[7] shl 24);
      Inc(c, pb[8] + pb[9] shl 8 + pb[10] shl 16 + pb[11] shl 24);
      Mix(a, b, c);
      Dec(Len, 12);
      Inc(pb, 12);
    end;

    case Len of
      0: Exit(Integer(c));
      1: goto case_1;
      2: goto case_2;
      3: goto case_3;
      4: goto case_4;
      5: goto case_5;
      6: goto case_6;
      7: goto case_7;
      8: goto case_8;
      9: goto case_9;
      10: goto case_10;
      11: goto case_11;
      12: goto case_12;
    end;

case_12:
    Inc(c, pb[11] shl 24);
case_11:
    Inc(c, pb[10] shl 16);
case_10:
    Inc(c, pb[9] shl 8);
case_9:
    Inc(c, pb[8]);
case_8:
    Inc(b, pb[7] shl 24);
case_7:
    Inc(b, pb[6] shl 16);
case_6:
    Inc(b, pb[5] shl 8);
case_5:
    Inc(b, pb[4]);
case_4:
    Inc(a, pb[3] shl 24);
case_3:
    Inc(a, pb[2] shl 16);
case_2:
    Inc(a, pb[1] shl 8);
case_1:
    Inc(a, pb[0]);
  end;

  Final(a, b, c);
  Result := Integer(c);
end;

{$POINTERMATH OFF}

class function THashBobJenkins.Create: THashBobJenkins;
begin
  Result.FHash := 0;
end;

function THashBobJenkins.GetDigest: TBytes;
begin
  SetLength(Result, 4);
  PCardinal(@Result[0])^ := THash.ToBigEndian(Cardinal(FHash));
end;

class function THashBobJenkins.GetHashString(const AString: string): string;
begin
  Result := Integer(GetHashValue(AString)).ToHexString(8);
end;

class function THashBobJenkins.GetHashValue(const AData: string): Integer;
begin
  Result := HashLittle(PChar(AData)^, AData.Length * SizeOf(Char), 0);
end;

class function THashBobJenkins.GetHashValue(const AData; ALength: Integer; AInitialValue: Integer): Integer;
begin
  Result := HashLittle(AData, ALength, AInitialValue);
end;

function THashBobJenkins.HashAsBytes: TBytes;
begin
  Result := GetDigest;
end;

function THashBobJenkins.HashAsInteger: Integer;
begin
  Result := FHash;
end;

function THashBobJenkins.HashAsString: string;
begin
  Result := FHash.ToHexString(8);
end;

class function THashBobJenkins.GetHashBytes(const AData: string): TBytes;
var
  LHash: Integer;
begin
  SetLength(Result, 4);
  LHash := HashLittle(PChar(AData)^, AData.Length * SizeOf(Char), 0);
  PCardinal(@Result[0])^ := THash.ToBigEndian(Cardinal(LHash));
end;

procedure THashBobJenkins.Reset;
begin
  FHash := 0;
end;

procedure THashBobJenkins.Update(const Input: string);
begin
  FHash := HashLittle(PChar(Input)^, Input.Length * SizeOf(Char), FHash);
end;

procedure THashBobJenkins.Update(const AData; ALength: Cardinal);
begin
  FHash := HashLittle(AData, ALength, FHash);
end;

procedure THashBobJenkins.Update(const AData: TBytes; ALength: Cardinal);
begin
  if ALength = 0 then
    ALength := Length(Adata);
  FHash := HashLittle(PByte(AData)^, ALength, FHash);
end;

{ THash }

class function THash.DigestAsInteger(const ADigest: TBytes): Integer;
begin
  if Length(ADigest) <> 4 then
    raise EHashException.Create('Digest size must be 4 to Generate a Integer');
  Result := PInteger(@ADigest[0])^;
end;

class function THash.DigestAsString(const ADigest: TBytes): string;
const
  XD: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
                              '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(ADigest) - 1 do
    Result := Result + XD[(ADigest[I] shr 4) and $0f] + XD[ADigest[I] and $0f];
end;

class function THash.DigestAsStringGUID(const ADigest: TBytes): string;
var
  LGUID: TGUID;
begin
  LGUID := TGUID.Create(ADigest);
  LGUID.D1 := ToBigEndian(LGUID.D1);
  LGUID.D2 := Word((WordRec(LGUID.D2).Lo shl 8) or WordRec(LGUID.D2).Hi);
  LGUID.D3 := Word((WordRec(LGUID.D3).Lo shl 8) or WordRec(LGUID.D3).Hi);
  Result := LGUID.ToString;
end;

class function THash.GetRandomString(const ALen: Integer): string;
const
  ValidChars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-/*_';
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ALen do
    Result := Result + ValidChars[Random(ValidChars.Length) + Low(string)];
end;

class function THash.ToBigEndian(AValue: Cardinal): Cardinal;
begin
  Result := (AValue shr 24) or (AValue shl 24) or ((AValue shr 8) and $FF00) or ((AValue shl 8) and $FF0000);
end;

end.
