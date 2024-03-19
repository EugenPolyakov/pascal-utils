unit DirectoryManager;

interface

uses SysUtils, StrUtils;

type
  TFileMask = record
  private type
    TMaskElementType = (metVal, metAst, metSym);
    TMaskElement = record
      Element: TMaskElementType;
      Value: string;
    end;
  private
    FMask: string;
    FProcessed: array of TMaskElement;
    procedure Initialize; inline;
  public
    function Compare(const Value: string): Boolean;
    function IsSimpleString: Boolean;
    procedure SetMask(const AMask: string);
    procedure CustomMask(const AMask, ASymbols: string);
    class operator Implicit(const AMask: string): TFileMask; static;
  end;

function ExpandFileNameEx(const Root, Path: string): string;
function CollapsePath(const FileName: string): string;
function IsSubDir(const Root, Path: string): Boolean;
function StrLIPos(SubStr, Str: PWideChar; LenSubStr, Len: Integer; Offset: Integer = 0): Integer; overload;
function StrLIPos(const SubStr, Str: string; Offset: Integer = 1): Integer; overload; inline;

implementation

function StrLIPos(const SubStr, Str: string; Offset: Integer): Integer;
begin
  Result:= StrLIPos(Pointer(SubStr), Pointer(Str), Length(SubStr), Length(Str), Offset - 1) + 1;
end;

function StrLIPos(SubStr, Str: PWideChar; LenSubStr, Len, Offset: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if LenSubStr = 0 then
    Exit;
  if Len = 0 then
    Exit;
  for I := Offset to Len - LenSubStr + Offset do
  begin
    if (Str[I] <> #0) and (AnsiStrLIComp(PWideChar(@Str[I]), SubStr, LenSubStr) = 0) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function LastDelimiterEx(const Delims, S: string; Pos: Integer): Integer;
var
  I, J: Integer;
begin
  I := Pos;
  while I >= Low(string) do
  begin
    for J := Low(string) to High(Delims) do
      if S[I] = Delims[J] then
        Exit(I);
    Dec(I);
  end;
  Result := -1;
end;

function ExpandFileNameEx(const Root, Path: string): string;
//var start, stop, l: Integer;
begin
  if Path = '' then
    Exit(Root);
  if IsRelativePath(Path) then begin
    Result:= CollapsePath(Root + Path);
  end //else if (Path[Low(string)] = PathDelim) and ((Length(Path) <= 1) or (Path[Low(string) + 1] <> PathDelim)) then
  //  Result:= ExtractFileDrive(Root) + Path
  else
    Result:= CollapsePath(Path);
end;

function CollapsePath(const FileName: string): string;
var i, j, s, last: Integer;
    relative: Integer;
begin
  SetLength(Result, Length(FileName));
  last:= High(FileName);
  s:= Low(string);
  i:= Low(string);
  relative:= Low(string);
  while i <= last do begin
    if (FileName[i] = '.') then begin
      if (i >= last) or (FileName[i + 1] = PathDelim) then begin
        Inc(i, 2);
        Continue;
      end else if (FileName[i + 1] = '.') and ((i + 1 >= last) or (FileName[i + 2] = PathDelim)) then begin
        if s > relative then begin
          j := s - 2;
          while j >= relative do begin
            if Result[j] = PathDelim then begin
              s:= j + 1;
              Break;
            end;
            Dec(j);
          end;
          if j = relative - 1 then //если не нашли удаляемый каталог
            s:= relative;
          Inc(i, 3);
          Continue;
        end;
        Inc(relative, 3);
      end
    end;
    //полностью копируем один уровень
    repeat
      Result[s]:= FileName[i];
      Inc(i);
      Inc(s);
    until (i > last) or (FileName[i - 1] = PathDelim);
  end;

  SetLength(Result, s - 2 + Low(string));
end;

function IsSubDir(const Root, Path: string): Boolean;
begin
  Result:= False;
end;

{ TFileMask }

function TFileMask.Compare(const Value: string): Boolean;
var
  I, ofs, J, K, E: Integer;
  p: PChar;
begin
  Initialize;
  ofs:= 0;
  p:= PChar(Value);
  E:= High(FProcessed);
  I:= 0;
  while I <= E do begin
    case FProcessed[I].Element of
      metVal: begin
        if AnsiStrLIComp(@p[ofs], PChar(FProcessed[I].Value), Length(FProcessed[I].Value)) = 0 then
          Inc(ofs, Length(FProcessed[I].Value))
        else
          Exit(False);
      end;
      metAst:
        if I < E then begin
          K:= I + 1;
          J:= 0;
          while (K <= E) and (FProcessed[K].Element <> metVal) do begin
            if FProcessed[K].Element = metSym then
              Inc(J);
            Inc(K);
          end;
          if K <= E then begin
            ofs:= StrLIPos(FProcessed[K].Value, Value, ofs + J + Low(string)) - Low(string);
            if ofs = -1 then
              Exit(False);
            Inc(ofs, Length(FProcessed[K].Value));
            I:= K;
          end else
            Exit(J + ofs <= Length(Value));
        end else
          Exit(True);
      metSym: begin
        if ofs < Length(Value) then
          Inc(ofs)
        else
          Exit(False);
      end;
    end;
    Inc(I);
  end;
  Result:= ofs = Length(Value);
end;

procedure TFileMask.CustomMask(const AMask, ASymbols: string);
var SymCount, AstCount, ValCount, Index: Integer;
    r: Boolean;
    bOfs, {eOfs,} l: Integer;
    //last: TMaskElementType;
    ast, sym: Char;
    esc: string;
  procedure AddValue(I: Integer);
  var j, c, m: Integer;
      p: PChar;
  begin
    if not r then begin
      FProcessed[Index].Element:= metVal;
      FProcessed[Index].Value:= Copy(FMask, bOfs, I - bOfs);
      if (l = 2) and (FProcessed[Index].Value <> '') then begin
        c:= 1;
        j:= 1;
        p:= PChar(FProcessed[Index].Value);
        m:= Length(FProcessed[Index].Value);
        while j < m do begin
          if (p[j] = esc) then begin
            p[c]:= p[j + 1];
            Inc(j);
          end else if c <> j then
            p[c]:= p[j];
          Inc(c);
          Inc(j);
        end;
        SetLength(FProcessed[Index].Value, c);
      end;
      Inc(Index);
    end;
  end;
var I: Integer;
begin
  if (Length(ASymbols) < 2) or (Length(ASymbols) > 3) then
    raise Exception.Create('Error Message');

  FMask:= AMask;
  FProcessed:= nil;

  SymCount:= 0;
  AstCount:= 0;
  ValCount:= 0;
  sym:= ASymbols[1];
  ast:= ASymbols[2];
  if Length(ASymbols) > 2 then begin
    esc:= ASymbols[3];
    l:= 2;
  end else
    l:= 0;
  r:= True;
  for I:= 1 to High(FMask) do
    if (FMask[I] = sym) and ((l = 0) or (I = 1) or (FMask[I - 1] <> esc)) then begin
      Inc(SymCount);
      r:= True;
    end else if (FMask[I] = ast) and ((l = 0) or (FMask[I - 1] <> esc)) then begin
      if (I = 1) or (FMask[I - 1] <> ast) or (I = l) or (FMask[I - 2] <> esc) then
        Inc(AstCount);
      r:= True;
    end else if r then begin
      Inc(ValCount);
      r:= False;
    end;
  SetLength(FProcessed, SymCount + AstCount + ValCount);

  r:= True;
  //last:= metVal;
  Index:= 0;
  for I:= 1 to High(FMask) do
    if (FMask[I] = sym) and ((l = 0) or (I = 1) or (FMask[I - 1] <> esc)) then begin
      AddValue(I);
      FProcessed[Index].Element:= metSym;
      Inc(Index);
      r:= True;
    end else if (FMask[I] = ast) and ((l = 0) or (FMask[I - 1] <> esc)) then begin
      if (I = 1) or (FMask[I - 1] <> ast) or (I = l) or (FMask[I - 2] <> esc) then begin
        AddValue(I);
        FProcessed[Index].Element:= metAst;
        Inc(Index);
      end;
      r:= True;
    end else if r then begin
      bOfs:= I;
      r:= False;
    end;
  AddValue(High(FMask) + 1);
end;

class operator TFileMask.Implicit(const AMask: string): TFileMask;
begin
  Result.SetMask(AMask);
end;

procedure TFileMask.Initialize;
begin
  if (FMask <> '') and (Length(FProcessed) = 0) then
    CustomMask(FMask, '?*');
end;

function TFileMask.IsSimpleString: Boolean;
begin
  Initialize;
  Result:= (FMask = '') or ((Length(FProcessed) = 1) and (FProcessed[0].Element = metVal));
end;

procedure TFileMask.SetMask(const AMask: string);
begin
  FMask:= AMask;
  SetLength(FProcessed, 0);
end;

end.
