unit DirectoryManager;

interface

uses SysUtils, StrUtils;

type
  TFileMask = record
  private type
    TMaskElementType = (metVal, metAst, metSym, metReset);
    TMaskElement = record
      Element: TMaskElementType;
      Value: string;
    end;
  private
    FMask: string;
    FProcessed: array of TMaskElement;
    procedure Initialize; inline;
  public
    property Mask: string read FMask;
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
  Result:= StrLIPos(Pointer(SubStr), Pointer(Str), Length(SubStr), Length(Str), Offset - 1) + Low(string);
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
  I, ofs, E: Integer;
  p: PChar;
begin
  Initialize;
  if Value = '' then
    Exit(FMask = '');
  ofs:= 0;
  p:= PChar(Pointer(Value));
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
          Inc(I);
          while I <= E do begin
            case FProcessed[I].Element of
              metVal: begin
                ofs:= StrLIPos(Pointer(FProcessed[I].Value), p, Length(FProcessed[I].Value), Length(Value), ofs);
                if ofs = -1 then begin
                  Inc(I);
                  while (I <= E) and (FProcessed[I].Element <> metReset) do
                    Inc(I);
                  if I <= E then //go to process Reset
                    Break;
                  Exit(False);
                end else
                  Inc(ofs, Length(FProcessed[I].Value));
                if I = E then
                  Exit(ofs = Length(Value));
                Inc(I);
                Break;
              end;
              metReset: Break;
              metSym: Inc(ofs);
            end;
            Inc(I);
          end;
          if I <= E then  //continue process
            Continue
          else
            Exit(ofs <= Length(Value));
        end else
          Exit(True);
      metSym: begin
        if ofs < Length(Value) then
          Inc(ofs)
        else
          Exit(False);
      end;
      metReset: begin
        if ofs = Length(Value) then
          Exit(True);
        ofs:= 0;
      end;
    end;
    Inc(I);
  end;
  Result:= ofs = Length(Value);
end;

procedure TFileMask.CustomMask(const AMask, ASymbols: string);
var cnt, Index: Integer;
    bOfs: Integer;
    last: TMaskElementType;
    ast, sym, reset, esc: Char;
    str: PChar;
  procedure AddValue(I: Integer);
  var j, c, m: Integer;
      p: PChar;
  begin
    if (last = metVal) and (I <> bOfs) then begin
      FProcessed[Index].Element:= metVal;
      SetString(FProcessed[Index].Value, PChar(@str[bOfs]), I - bOfs);
      if esc <> #0 then begin
        c:= 0;
        j:= 0;
        p:= PChar(Pointer(FProcessed[Index].Value));
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
var I, len: Integer;
begin
  if (Length(ASymbols) < 2) or (Length(ASymbols) > 4) then
    raise Exception.Create('Error Message');

  FMask:= AMask;
  FProcessed:= nil;

  if FMask = '' then
    Exit;

  str:= Pointer(FMask);
  sym:= ASymbols[1];
  ast:= ASymbols[2];
  if Length(ASymbols) > 2 then
    reset:= ASymbols[3]
  else
    reset:= #0;
  if Length(ASymbols) > 3 then
    esc:= ASymbols[4]
  else
    esc:= #0;

  I:= 0;
  if str[0] = sym then
    last:= metSym
  else if str[0] = ast then
    last:= metAst
  else if str[0] = reset then
    last:= metReset
  else begin
    if str[0] = esc then
      Inc(I);
    last:= metVal
  end;
  Inc(I);

  cnt:= 1;
  len:= Length(FMask);
  while I < len do begin
    if str[I] = sym then begin
      Inc(cnt);
      last:= metSym;
    end else if str[I] = ast then begin
      if last <> metAst then begin
        Inc(cnt);
        last:= metAst;
      end;
    end else if str[I] = reset then begin
      if last <> metReset then begin
        Inc(cnt);
        last:= metReset;
      end;
    end else begin
      if last <> metVal then begin
        Inc(cnt);
        last:= metVal;
      end;
      if str[I] = esc then
        Inc(I);
    end;
    Inc(I);
  end;

  SetLength(FProcessed, cnt);

  last:= metVal;
  bOfs:= 0;
  I:= 0;
  Index:= 0;
  while I < len do begin
    if str[I] = sym then begin
      AddValue(I);
      FProcessed[Index].Element:= metSym;
      Inc(Index);
      last:= metSym;
    end else if str[I] = ast then begin
      if last <> metAst then begin
        AddValue(I);
        FProcessed[Index].Element:= metAst;
        Inc(Index);
        last:= metAst;
      end;
    end else if str[I] = reset then begin
      if last <> metReset then begin
        AddValue(I);
        FProcessed[Index].Element:= metReset;
        Inc(Index);
        last:= metReset;
      end;
    end else begin
      if last <> metVal then begin
        bOfs:= I;
        last:= metVal;
      end;
      if str[I] = esc then
        Inc(I);
    end;
    Inc(I);
  end;
  AddValue(High(FMask));
end;

class operator TFileMask.Implicit(const AMask: string): TFileMask;
begin
  Result.SetMask(AMask);
end;

procedure TFileMask.Initialize;
begin
  if (FMask <> '') and (Length(FProcessed) = 0) then
    CustomMask(FMask, '?*;');
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
