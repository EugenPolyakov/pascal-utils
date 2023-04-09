unit ExceptionStackTrace;

interface

type
  TStackInfo = record
    Count: Integer;
    StackTrace: array [0..0] of Pointer;
  end;
  PStackInfo = ^TStackInfo;

const
  cNonDelphiException = $0EEDFAE4;
  cDelphiException    = $0EEDFADE;
  cContinuable        = 0;
  MAX_STACK_LENGTH = 16;

function GetExceptionStackInfoProc(P: System.PExceptionRecord): Pointer;
function GetStackInfoStringProc(Info: Pointer): string;
procedure CleanUpStackInfoProc(Info: Pointer);

implementation

uses System.SysUtils, Winapi.Windows, SysTypes, WinAPIExtensions;

{$R-}

function GetExceptionStackInfoProc(P: System.PExceptionRecord): Pointer;
var
  rets: array [0..MAX_STACK_LENGTH - 1] of Pointer;
  i: Integer;
  Count: Integer;
begin
  if P.ExceptionCode = cDelphiException then begin
    Count:= CaptureStackBackTrace({$IFNDEF CPUX64}2{$ELSE}6{$ENDIF}, 16, @rets[0], nil);
  end else begin
    {$MESSAGE WARN 'Only XE7 code'}
    {$IFNDEF CPUX64}
    Count:= CaptureStackBackTrace(5, 16, @rets[0], nil);
    //from _HandleOnException we have broken stack
    if Count = 0 then
    asm
      //get EBP of GetExceptionObject
      MOV EAX, [EBP]
      //get real ebp of _HandleOnException
      MOV EAX, [EAX + 8]
      MOV EDX, EAX
      PUSH EBP
      PUSH 0
      LEA EAX, rets[0]
      PUSH EAX
      PUSH 16
      PUSH 3
      MOV EBP, EDX
      CALL CaptureStackBackTrace
      POP EBP
      MOV Count, EAX
    end;
    {$ELSE}
    Count:= CaptureStackBackTrace(8, 16, @rets[0], nil);
    {$ENDIF}
  end;
  GetMem(Result, SizeOf(TStackInfo) + Count * SizeOf(Pointer));
  PStackInfo(Result).Count:= Count + 1;
  PStackInfo(Result).StackTrace[0]:= P.ExceptionAddress;
  i:= 1;
  Move(rets[0], PStackInfo(Result).StackTrace[i], Count * SizeOf(Pointer));
end;

function GetStackInfoStringProc(Info: Pointer): string;
var
  i: Integer;
begin
  if Info <> nil then
  for i := 0 to PStackInfo(Info).Count - 1 do
    Result:= Format({$IFNDEF CPUX64}'%s' + sLineBreak + '%.8X'{$ELSE}'%s' + sLineBreak + '%.16X'{$ENDIF}, [Result, NativeInt(PStackInfo(Info).StackTrace[i])]);
end;

procedure CleanUpStackInfoProc(Info: Pointer);
begin
  FreeMem(Info);
end;

initialization

  Exception.GetExceptionStackInfoProc:= GetExceptionStackInfoProc;
  Exception.GetStackInfoStringProc:= GetStackInfoStringProc;
  Exception.CleanUpStackInfoProc:= CleanUpStackInfoProc;

finalization

end.
