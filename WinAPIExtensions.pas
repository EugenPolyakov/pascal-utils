unit WinAPIExtensions;

interface

uses
  System.SysUtils, SysTypes, Winapi.Windows, Winapi.ShellAPI, Winapi.WinSock2, Winapi.ActiveX;

type
  TRecordFinalizator = procedure of object;

  TWindowsVersion = (wvUnknownVersion, wvWindows2000, wvWindowsXP, wvWindowsXP64,
      wvWindows2003 = wvWindowsXP64, wvWindows2003R2 = wvWindows2003,
      wvWindowsVista, wvWindows2008 = wvWindowsVista,
      wvWindows7, wvWindows2008R2 = wvWindows7,
      wvWindows8, wvWindows2012 = wvWindows8,
      wvWindows8_1, wvWindows2012R2 = wvWindows8_1,
      wvWindows10, wvWindows2016 = wvWindows10);

  TWindowsInfo = record
  private
    class var FWindowsVersion: TWindowsVersion;
    class constructor Create;
  public
    class property WindowsVersion: TWindowsVersion read FWindowsVersion;
  end;

  TDropFile = record
  private
    FDrop: HDROP;
    FFinalizator: TAutoFinalizedRecord;
    procedure Finalize;
    function GetFile(Index: Integer): string;
    function GetFileCount: Integer;
  public
    property FileCount: Integer read GetFileCount;
    property FileName[Index: Integer]: string read GetFile;
    property Drop: HDROP read FDrop;
    constructor Create(ADrop: HDROP);
  end;

  TWatcher = record
  private
    FTime: Int64;
  public
    procedure BeginWatch;
    function GetWatch10kOfSec: Integer;
    function GetWatch: Extended;
  end;

  TInAddr = in_addr;
  PInAddr = ^in_addr;

  in6_addr = record
    case Integer of
      0: (Byte: array [0..15] of u_char);
      1: (Word: array [0..7] of u_short);
  end;
  TIn6Addr = in6_addr;
  PIn6Addr = ^in6_addr;

  sockaddr_in6 = record
    sin6_family: Smallint;
    sin6_port: u_short;
    sin6_flowinfo: u_long;
    sin6_addr: in6_addr;
    sin6_scope_id: u_long;
  end;
  TSockAddrIn6 = sockaddr_in6;
  PSockAddrIn6 = ^sockaddr_in6;

  TSockAddr = record
  case Integer of
    0: (Addr: Winapi.WinSock2.TSockAddr);
    1: (AddrIn4: TSockAddrIn);
    2: (AddrIn6: TSockAddrIn6);
  end;

  TAppExecutionOutputEvent = procedure(const ACmdStr: string;
                                        AProcessID: DWORD;
                                        const AOutputTextPart: string;
                                        const AAnyID: string;
                                        AActDurationMSec: DWORD;
                                        var ADoTerminating: boolean) of object;

const
  SC_DRAGMOVE = $F012;

  LABEL_SECURITY_INFORMATION = $00000010;
  ATTRIBUTE_SECURITY_INFORMATION = $00000020;
  SCOPE_SECURITY_INFORMATION = $00000040;
  PROCESS_TRUST_LABEL_SECURITY_INFORMATION = $00000080;
  BACKUP_SECURITY_INFORMATION = $00010000;
  UNPROTECTED_SACL_SECURITY_INFORMATION = $10000000;
  UNPROTECTED_DACL_SECURITY_INFORMATION = $20000000;
  PROTECTED_SACL_SECURITY_INFORMATION = $40000000;
  PROTECTED_DACL_SECURITY_INFORMATION = $80000000;

  ATTACH_PARENT_PROCESS = DWORD(-1);

  IF_TYPE_OTHER = 1; //Some other type of network interface.
  IF_TYPE_ETHERNET_CSMACD = 6; //An Ethernet network interface.
  IF_TYPE_ISO88025_TOKENRING = 9; //A token ring network interface.
  IF_TYPE_PPP = 23; //A PPP network interface.
  IF_TYPE_SOFTWARE_LOOPBACK = 24; //A software loopback network interface.
  IF_TYPE_ATM = 37; //An ATM network interface.
  IF_TYPE_IEEE80211 = 71; //An IEEE 802.11 wireless network interface.
                          //On Windows Vista and later, wireless network cards are reported as IF_TYPE_IEEE80211. On earlier versions of Windows, wireless network cards are reported as IF_TYPE_ETHERNET_CSMACD.
                          //On Windows XP with SP3 and on Windows XP with SP2 x86 with the Wireless LAN API for Windows XP with SP2 installed, the WlanEnumInterfaces function can be used to enumerate wireless interfaces on the local computer.
  IF_TYPE_TUNNEL = 131; //A tunnel type encapsulation network interface.
  IF_TYPE_IEEE1394 = 144; //

  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege'; //Required to assign the primary token of a process.
                                                                //User Right: Replace a process-level token.
  SE_AUDIT_NAME = 'SeAuditPrivilege'; //Required to generate audit-log entries. Give this privilege to secure servers.
                                      //User Right: Generate security audits.
  SE_BACKUP_NAME = 'SeBackupPrivilege'; //Required to perform backup operations. This privilege causes the system to grant all read access control to any file, regardless of the access control list (ACL) specified for the file. Any access request other than read is still evaluated with the ACL. This privilege is required by the RegSaveKey and RegSaveKeyExfunctions. The following access rights are granted if this privilege is held:
                                        //READ_CONTROL
                                        //ACCESS_SYSTEM_SECURITY
                                        //FILE_GENERIC_READ
                                        //FILE_TRAVERSE
                                        //User Right: Back up files and directories.
                                        //If the file is located on a removable drive and the "Audit Removable Storage" is enabled, the SE_SECURITY_NAME is required to have ACCESS_SYSTEM_SECURITY.
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege'; //Required to receive notifications of changes to files or directories. This privilege also causes the system to skip all traversal access checks. It is enabled by default for all users.
                                                     //User Right: Bypass traverse checking.
  SE_CREATE_GLOBAL_NAME = 'SeCreateGlobalPrivilege'; //Required to create named file mapping objects in the global namespace during Terminal Services sessions. This privilege is enabled by default for administrators, services, and the local system account.
                                                     //User Right: Create global objects.
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege'; //Required to create a paging file.
                                                         //User Right: Create a pagefile.
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege'; //Required to create a permanent object.
                                                           //User Right: Create permanent shared objects.
  SE_CREATE_SYMBOLIC_LINK_NAME = 'SeCreateSymbolicLinkPrivilege'; //Required to create a symbolic link.
                                                                  //User Right: Create symbolic links.
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege'; //Required to create a primary token.
                                                   //User Right: Create a token object.
                                                   //You cannot add this privilege to a user account with the "Create a token object" policy. Additionally, you cannot add this privilege to an owned process using Windows APIs.Windows Server 2003 and Windows XP with SP1 and earlier: Windows APIs can add this privilege to an owned process.
  SE_DEBUG_NAME = 'SeDebugPrivilege'; //Required to debug and adjust the memory of a process owned by another account.
                                      //User Right: Debug programs.
  SE_DELEGATE_SESSION_USER_IMPERSONATE_NAME = 'SeDelegateSessionUserImpersonatePrivilege'; //Required to obtain an impersonation token for another user in the same session.
                                                                                           //User Right: Impersonate other users.
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege'; //Required to mark user and computer accounts as trusted for delegation.
                                                             //User Right: Enable computer and user accounts to be trusted for delegation.
  SE_IMPERSONATE_NAME = 'SeImpersonatePrivilege'; //Required to impersonate.
                                                  //User Right: Impersonate a client after authentication.
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege'; //Required to increase the base priority of a process.
                                                                 //User Right: Increase scheduling priority.
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege'; //Required to increase the quota assigned to a process.
                                                       //User Right: Adjust memory quotas for a process.
  SE_INC_WORKING_SET_NAME = 'SeIncreaseWorkingSetPrivilege'; //Required to allocate more memory for applications that run in the context of users.
                                                             //User Right: Increase a process working set.
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege'; //Required to load or unload a device driver.
                                                 //User Right: Load and unload device drivers.
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege'; //Required to lock physical pages in memory.
                                                 //User Right: Lock pages in memory.
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege'; //Required to create a computer account.
                                                         //User Right: Add workstations to domain.
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege'; //Required to enable volume management privileges.
                                                     //User Right: Manage the files on a volume.
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege'; //Required to gather profiling information for a single process.
                                                                   //User Right: Profile single process.
  SE_RELABEL_NAME = 'SeRelabelPrivilege'; //Required to modify the mandatory integrity level of an object.
                                          //User Right: Modify an object label.
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege'; //Required to shut down a system using a network request.
                                                         //User Right: Force shutdown from a remote system.
  SE_RESTORE_NAME = 'SeRestorePrivilege'; //Required to perform restore operations. This privilege causes the system to grant all write access control to any file, regardless of the ACL specified for the file. Any access request other than write is still evaluated with the ACL. Additionally, this privilege enables you to set any valid user or group SID as the owner of a file. This privilege is required by the RegLoadKey function. The following access rights are granted if this privilege is held:
                                          //WRITE_DAC
                                          //WRITE_OWNER
                                          //ACCESS_SYSTEM_SECURITY
                                          //FILE_GENERIC_WRITE
                                          //FILE_ADD_FILE
                                          //FILE_ADD_SUBDIRECTORY
                                          //DELETE
                                          //User Right: Restore files and directories.
                                          //If the file is located on a removable drive and the "Audit Removable Storage" is enabled, the SE_SECURITY_NAME is required to have ACCESS_SYSTEM_SECURITY.
  SE_SECURITY_NAME = 'SeSecurityPrivilege'; //Required to perform a number of security-related functions, such as controlling and viewing audit messages. This privilege identifies its holder as a security operator.
                                            //User Right: Manage auditing and security log.
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege'; //Required to shut down a local system.
                                            //User Right: Shut down the system.
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege'; //Required for a domain controller to use the Lightweight Directory Access Protocol directory synchronization services. This privilege enables the holder to read all objects and properties in the directory, regardless of the protection on the objects and properties. By default, it is assigned to the Administrator and LocalSystem accounts on domain controllers.
                                               //User Right: Synchronize directory service data.
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege'; //Required to modify the nonvolatile RAM of systems that use this type of memory to store configuration information.
                                                               //User Right: Modify firmware environment values.
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege'; //Required to gather profiling information for the entire system.
                                                       //User Right: Profile system performance.
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege'; //Required to modify the system time.
                                                //User Right: Change the system time.
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege'; //Required to take ownership of an object without being granted discretionary access. This privilege allows the owner value to be set only to those values that the holder may legitimately assign as the owner of an object.
                                                       //User Right: Take ownership of files or other objects.
  SE_TCB_NAME = 'SeTcbPrivilege'; //This privilege identifies its holder as part of the trusted computer base. Some trusted protected subsystems are granted this privilege.
                                  //User Right: Act as part of the operating system.
  SE_TIME_ZONE_NAME = 'SeTimeZonePrivilege'; //Required to adjust the time zone associated with the computer's internal clock.
                                             //User Right: Change the time zone.
  SE_TRUSTED_CREDMAN_ACCESS_NAME = 'SeTrustedCredManAccessPrivilege'; //Required to access Credential Manager as a trusted caller.
                                                                      //User Right: Access Credential Manager as a trusted caller.
  SE_UNDOCK_NAME = 'SeUndockPrivilege'; //Required to undock a laptop.
                                        //User Right: Remove computer from docking station.
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege'; //Required to read unsolicited input from a terminal device.
                                                             //User Right: Not applicable.

procedure AddDropFilesExStyle(var ExStyle: DWORD);
function FillSockAddr(const Address: string; var addr: TSockAddr): Boolean; overload;
function FillSockAddr(const Address: AnsiString; var addr: TSockAddr): Boolean; overload;
function StrToIPv4(Str: PAnsiChar; var addr: TInAddr; var err: Integer): Boolean; overload;
function StrToIPv4(Str: PWideChar; var addr: TInAddr; var err: Integer): Boolean; overload;
function StrToIPv4(const Str: AnsiString; var addr: TInAddr; var err: Integer): Boolean; overload; inline;
function StrToIPv4(const Str: string; var addr: TInAddr; var err: Integer): Boolean; overload; inline;
function StrToIPv6(Str: PAnsiChar; var addr: TIn6Addr; var err: Integer): Boolean; overload;
function StrToIPv6(Str: PWideChar; var addr: TIn6Addr; var err: Integer): Boolean; overload;
function StrToIPv6(const Str: AnsiString; var addr: TIn6Addr; var err: Integer): Boolean; overload; inline;
function StrToIPv6(const Str: string; var addr: TIn6Addr; var err: Integer): Boolean; overload; inline;
procedure HexStrToInt(Str: PAnsiChar; var Value, Err: Integer); overload;
procedure HexStrToInt(Str: PWideChar; var Value, Err: Integer); overload;
procedure HexStrToInt(const Str: AnsiString; var Value, Err: Integer); overload; inline;
procedure HexStrToInt(const Str: string; var Value, Err: Integer); overload; inline;

procedure WinExec(const ACmdLine: string; const ACmdShow: UINT = SW_SHOWNORMAL; AWait: DWORD = 0);
procedure StartProgram(const AAppName, AParams: string; AWait: DWORD = 0; ACmdShow: UINT = SW_SHOWNORMAL);
procedure ShellExecute(const AWnd: HWND; const AOperation, AFileName: string; const AParameters: string = ''; const ADirectory: string = ''; const AShowCmd: Integer = SW_SHOWNORMAL);
procedure ShowObjectInExplorer(const AWnd: HWND; const APath: string);
procedure ExecAppAndGetOutput(const ACmdStr: string;
                              //AOutput: TStrings;  //опционально: сразу вывод в TStrings
                              AOutputEvent: TAppExecutionOutputEvent; //опционально: обработчик события вывода с вкусняшками
                              const AAnyID: string); //опционально: строка, передаваемая в AOutputEvent

function GetFileVersion(Instance: HINST): TVSFixedFileInfo;
function GetFileVersionString(Instance: HINST): string;
function GetCurrentExeVersion: TVSFixedFileInfo;
function GetCurrentExeVersionString: string;

function AttachConsole(dwProcessId: DWORD): BOOL; stdcall;
function AllocConsole: BOOL; stdcall;
function FreeConsole: BOOL; stdcall;
function CaptureStackBackTrace(FramesToSkip, FramesToCapture: ULONG; BackTrace: PPointer; BackTraceHash: PULONG): USHORT; stdcall;

function SetCurrentPrivilege(lpszPrivilege: LPCTSTR; bEnablePrivilege: BOOL): BOOL;
function SetPrivilege(
     hToken: THandle;         // access token handle
     lpszPrivilege: LPCTSTR;  // name of privilege to enable/disable
     bEnablePrivilege: BOOL   // to enable or disable privilege
    ): BOOL;

implementation

function AttachConsole; external kernel32 name 'AttachConsole';
function AllocConsole; external kernel32 name 'AllocConsole';
function FreeConsole; external kernel32 name 'FreeConsole';
function CaptureStackBackTrace; external kernel32 name 'RtlCaptureStackBackTrace';

var Frequency: Int64;

function SetCurrentPrivilege(lpszPrivilege: LPCTSTR; bEnablePrivilege: BOOL): BOOL;
var hToken: THandle;
begin
  if not OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
    Result:= False
  else begin
    Result:= SetPrivilege(hToken, lpszPrivilege, bEnablePrivilege);
    CloseHandle(hToken);
  end;
end;

function SetPrivilege(hToken: THandle; lpszPrivilege: LPCTSTR; bEnablePrivilege: BOOL): BOOL;
var tkp: TTokenPrivileges;
begin
  if LookupPrivilegeValue(nil, lpszPrivilege, tkp.Privileges[0].Luid) then begin
    tkp.PrivilegeCount := 1;
    if bEnablePrivilege then
      tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
    else
      tkp.Privileges[0].Attributes := 0;
    Result:= AdjustTokenPrivileges(hToken, False, tkp, SizeOf(tkp), nil, DWORD(nil^));
  end else
    Result:= False;
end;

procedure ShowObjectInExplorer(const AWnd: HWND; const APath: string);
begin
  ShellExecute(AWnd, 'OPEN', 'EXPLORER', Format('/select, "%s"', [APath]));
end;

function ReadScopeIPv6(const Address: AnsiString; ofs: Integer; var addr: TSockAddrIn6): Integer; overload;
var str: PAnsiChar;
begin
  str:= @(PAnsiChar(Pointer(Address))[ofs]);
  addr.sin6_scope_id:= ValAnsi(str, Result);
  addr.sin6_scope_id:= htons(addr.sin6_scope_id);
  if Result = 0 then
    Result:= Length(Address)
  else
    Inc(Result, ofs);
end;

function FillSockAddr(const Address: AnsiString; var addr: TSockAddr): Boolean;
var addr4: TInAddr;
    err: Integer;
    str: PAnsiChar;
begin
  FillChar(addr, SizeOf(addr), 0);
  str:= Pointer(Address);
  if (str[0] <> '[') and StrToIPv4(str, addr4, err) then begin
    addr.AddrIn4.sin_family:= AF_INET;
    addr.AddrIn4.sin_addr:= addr4;
    Result:= True;
  end else begin
    if str[0] = '[' then begin
      Result:= StrToIPv6(PAnsiChar(@str[1]), addr.AddrIn6.sin6_addr, err);

      if str[err] = '%' then
        err:= ReadScopeIPv6(Address, err + 1, addr.AddrIn6);

      if str[err] <> ']' then
        Exit(False);
      Inc(err);
    end else begin
      Result:= StrToIPv6(str, addr.AddrIn6.sin6_addr, err);
      if str[err] = '%' then
        err:= ReadScopeIPv6(Address, err + 1, addr.AddrIn6);
    end;
    if Result then begin
      addr.AddrIn6.sin6_family:= AF_INET6;
    end else
      Exit(False);
  end;

  if str[err] = ':' then begin
    addr.AddrIn4.sin_port:= ValAnsi(PAnsiChar(@str[err + 1]), err);
    addr.AddrIn4.sin_port:= htons(addr.AddrIn4.sin_port);
    Result:= err = 0;
  end;
end;

function ReadScopeIPv6(const Address: string; ofs: Integer; var addr: TSockAddrIn6): Integer; overload;
var str: PWideChar;
begin
  str:= @(PWideChar(Pointer(Address))[ofs]);
  val(str, addr.sin6_scope_id, Result);
  addr.sin6_scope_id:= htons(addr.sin6_scope_id);
  if Result = 0 then
    Result:= Length(Address)
  else
    Inc(Result, ofs);
end;

function FillSockAddr(const Address: string; var addr: TSockAddr): Boolean;
var addr4: TInAddr;
    err, port: Integer;
    str: PChar;
begin
  FillChar(addr, SizeOf(addr), 0);
  str:= Pointer(Address);
  if (str[0] <> '[') and StrToIPv4(str, addr4, err) then begin
    addr.AddrIn4.sin_family:= AF_INET;
    addr.AddrIn4.sin_addr:= addr4;
    Result:= True;
  end else begin
    if str[0] = '[' then begin
      Result:= StrToIPv6(PWideChar(@str[1]), addr.AddrIn6.sin6_addr, err);

      if str[err] = '%' then
        err:= ReadScopeIPv6(Address, err + 1, addr.AddrIn6);

      if str[err] <> ']' then
        Exit(False);
      Inc(err);
    end else begin
      Result:= StrToIPv6(str, addr.AddrIn6.sin6_addr, err);
      if str[err] = '%' then
        err:= ReadScopeIPv6(Address, err + 1, addr.AddrIn6);
    end;
    if Result then begin
      addr.AddrIn6.sin6_family:= AF_INET6;
    end else
      Exit(False);
  end;

  if str[err] = ':' then begin
    val(string(@str[err + 1]), port, err);
    if (port < 0) or (port > Word.MaxValue) then
      Exit(False);
    addr.AddrIn4.sin_port:= port;
    addr.AddrIn4.sin_port:= htons(addr.AddrIn4.sin_port);
    Result:= err = 0;
  end;
end;

//совет нашёл здесь  https://stackoverflow.com/questions/13816962
//для предотвращения зависания ReadFile(PipeHandle)
//это происходит, например, при убитии процесса
function CheckIfPipeHasData(APipeHandle: THandle; AOnlyUnreadData: boolean): boolean;
const
   READ_BUFFER_SIZE  = 10;
var
   arrBuffer         : array[0..READ_BUFFER_SIZE-1] of AnsiChar;
   iBytesRead        : DWORD;
   iBytesAvailable   : DWORD;
   iBytesUnread      : DWORD;
begin
   //эта функция не зависнет, это важно. В отличие от ReadFile
   if PeekNamedPipe(APipeHandle,          // HANDLE hNamedPipe = handle to pipe to copy from
                    @arrBuffer,           // LPVOID lpBuffer = pointer to data buffer
                    READ_BUFFER_SIZE,     // DWORD nBufferSize = size, in bytes, of data buffer
                    @iBytesRead,          // LPDWORD lpBytesRead = pointer to number of bytes read
                    @iBytesAvailable,     // LPDWORD lpTotalBytesAvail = pointer to total number of bytes available
                    @iBytesUnread         // LPDWORD lpBytesLeftThisMessage = pointer to unread bytes in this message
                   ) then
   begin
      if AOnlyUnreadData then
         Result := (iBytesUnread > 0)
      else
         Result := (iBytesAvailable > 0);
   end
   else
   begin
      Result := false;
   end;
end;

//------------------------------------------------------------------------------
//базируется на RunDosInMemo() с https://stackoverflow.com/questions/9119999
//с моей доработкой
procedure ExecAppAndGetOutput(const ACmdStr: string; //AOutput: TStrings;
  AOutputEvent: TAppExecutionOutputEvent; const AAnyID: string);
const
   READ_BUFFER_SIZE     = 2400;
var
   iExecutionStartMSec  : DWORD;
   sCmdStr              : string;
   Security             : TSecurityAttributes;
   readableEndOfPipe    : THandle;
   writeableEndOfPipe   : THandle;
   start                : TStartUpInfo;
   Buffer               : PAnsiChar;
   ProcessInfo          : TProcessInformation;
   iAppRunningFlag      : DWORD;
   bDoTerminating       : boolean;

   (**) procedure PushOutput(out ADoTerminating: boolean);
   (**) var
   (**)    iBytesRead : DWORD;
   (**) begin
   (**)    ADoTerminating := false;
   (**)    //Read the contents of the pipe out of the readable end
   (**)    //WARNING: if the console app never writes anything to the StdOutput, then ReadFile will block and never return
   (**)    repeat
   (**)       iBytesRead := 0;
   (**)       if CheckIfPipeHasData(readableEndOfPipe, true) then //<<< эта проверка предотвращает зависание (моя находка)
   (**)       begin
   (**)          ReadFile(readableEndOfPipe, Buffer[0], READ_BUFFER_SIZE, {var}iBytesRead, nil);
   (**)          Buffer[iBytesRead]:= #0;
   (**)          OemToAnsi(Buffer,Buffer);
   (**)       end
   (**)       else
   (**)       begin
   (**)          iBytesRead := 0;
   (**)          Buffer[iBytesRead]:= #0;
   (**)       end;
   (**)
   (**)       //if Assigned(AOutput) then
   (**)       //   AOutput.Text := AOutput.Text + String(Buffer);
   (**)
   (**)       if Assigned(AOutputEvent) then
   (**)          AOutputEvent(ACmdStr,
   (**)                       ProcessInfo.dwProcessId,
   (**)                       String(Buffer),
   (**)                       AAnyID,
   (**)                       GetTickCount()-iExecutionStartMSec,
   (**)                       ADoTerminating);
   (**)    until ADoTerminating or (iBytesRead < READ_BUFFER_SIZE);
   (**) end;
begin
  iExecutionStartMSec := GetTickCount();

  sCmdStr := ACmdStr;

  Security.nLength := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;

  if CreatePipe({var}readableEndOfPipe, {var}writeableEndOfPipe, @Security, 0) then
  begin
    Buffer := AllocMem(READ_BUFFER_SIZE+1);
    try
      FillChar(Start, SizeOf(start), #0);
      start.cb := SizeOf(start);

      // Set up members of the STARTUPINFO structure.
      // This structure specifies the STDIN and STDOUT handles for redirection.
      // - Redirect the output and error to the writeable end of our pipe.
      // - We must still supply a valid StdInput handle (because we used STARTF_USESTDHANDLES to swear that all three handles will be valid)
      start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
      start.hStdInput := GetStdHandle(STD_INPUT_HANDLE); //we're not redirecting stdInput; but we still have to give it a valid handle
      start.hStdOutput := writeableEndOfPipe; //we give the writeable end of the pipe to the child process; we read from the readable end
      start.hStdError := writeableEndOfPipe;

      //We can also choose to say that the wShowWindow member contains a value.
      //In our case we want to force the console window to be hidden.
      start.dwFlags := start.dwFlags + STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;

      // Don't forget to set up members of the PROCESS_INFORMATION structure.
      //--- ProcessInfo := Default(TProcessInformation);
      FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);


      //WARNING: The unicode version of CreateProcess (CreateProcessW) can modify the command-line "DosApp" string.
      //Therefore "DosApp" cannot be a pointer to read-only memory, or an ACCESS_VIOLATION will occur.
      //We can ensure it's not read-only with the RTL function: UniqueString
      UniqueString({var}sCmdStr);

      try
        if CreateProcess(nil, PChar(sCmdStr), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, start, {var}ProcessInfo) then
        begin
          bDoTerminating := false;

          //Wait for the application to terminate, as it writes it's output to the pipe.
          //WARNING: If the console app outputs more than 2400 bytes (ReadBuffer),
          //it will block on writing to the pipe and *never* close.
          repeat
            iAppRunningFlag := WaitForSingleObject(ProcessInfo.hProcess, 100);

            if iAppRunningFlag = WAIT_TIMEOUT then
               PushOutput(bDoTerminating);

          until bDoTerminating or (iAppRunningFlag <> WAIT_TIMEOUT);

          //под конец считываем что там ещё осталось в трубке
          if not bDoTerminating then
            PushOutput(bDoTerminating);
        end;
      finally
        if bDoTerminating then
          TerminateProcess(ProcessInfo.hProcess, 0)
        else
        begin
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
        end;

        CloseHandle(readableEndOfPipe);
        CloseHandle(writeableEndOfPipe);
      end;
    finally
      FreeMem(Buffer);
    end;
  end; //if CreatePipe
end;

function GetFileVersion(Instance: HINST): TVSFixedFileInfo;
var InfoSize: DWORD;
    FI: PVSFixedFileInfo;
    HResource: TResourceHandle;
    HResData: THandle;
    PRes: Pointer;
begin
  FillChar(Result, SizeOf(Result), 0);
  HResource := FindResource(Instance, MakeIntResource(VS_VERSION_INFO), RT_VERSION);
  if HResource <> 0 then begin
    HResData:= LoadResource(Instance, HResource);
    if HResData <> 0 then begin
      PRes:= LockResource(HResData);
      if Assigned(PRes) then begin
        InfoSize := SizeofResource(HInstance, HResource);
        if InfoSize <> 0 then
          if VerQueryValue(PRes, '\', Pointer(FI), InfoSize) then begin
            Result:= FI^;
          end;
      end;
//      UnlockResource(HResData); // unnecessary per MSDN
//      FreeResource(HResData);   // unnecessary per MSDN
    end;
  end;
end;

function GetFileVersionString(Instance: HINST): string;
begin
  with GetFileVersion(Instance) do
    Result:= Format('%d.%d.%d.%d', [LongRec(dwFileVersionMS).Hi,
        LongRec(dwFileVersionMS).Lo, LongRec(dwFileVersionLS).Hi, LongRec(dwFileVersionLS).Lo]);
end;

function GetCurrentExeVersion: TVSFixedFileInfo;
begin
  Result:= GetFileVersion(HInstance);
end;

function GetCurrentExeVersionString: string;
begin
  Result:= GetFileVersionString(HInstance);
end;

procedure ShellExecute(const AWnd: HWND; const AOperation, AFileName, AParameters, ADirectory: string; const AShowCmd: Integer);
var
  ExecInfo: TShellExecuteInfo;
  NeedUnitialize: Boolean;
begin
  Assert(AFileName <> '');

  NeedUnitialize := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := AWnd;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_NOASYNC { = SEE_MASK_FLAG_DDEWAIT для старых версий Delphi }
                   or SEE_MASK_FLAG_NO_UI;
    {$IFDEF UNICODE}
    // Необязательно, см. http://www.transl-gunsmoker.ru/2015/01/what-does-SEEMASKUNICODE-flag-in-ShellExecuteEx-actually-do.html
    ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_UNICODE;
    {$ENDIF}

    {$WARN SYMBOL_PLATFORM OFF}
    Win32Check(ShellExecuteEx(@ExecInfo));
    {$WARN SYMBOL_PLATFORM ON}
  finally
    if NeedUnitialize then
      CoUninitialize;
  end;
end;

procedure WinExec(const ACmdLine: string; const ACmdShow: UINT; AWait: DWORD);
var
  SI: TStartupInfo;
  PI: TProcessInformation;
begin
  FillChar(SI, SizeOf(SI), 0);
  FillChar(PI, SizeOf(PI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := ACmdShow;

  SetLastError(ERROR_INVALID_PARAMETER);
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(CreateProcess(nil, PWideChar(ACmdLine), nil, nil, False,
      CREATE_DEFAULT_ERROR_MODE or CREATE_UNICODE_ENVIRONMENT, nil, nil, SI, PI));
  {$WARN SYMBOL_PLATFORM ON}

  if AWait <> 0 then begin
    //Ждем завершения инициализации.
    WaitForInputIdle(PI.hProcess, AWait);
    //Ждем завершения процесса.
    WaitforSingleObject(PI.hProcess, AWait);
    //Получаем код завершения.
    //GetExitCodeProcess(PI.hProcess, ExitCode);
  end;

  CloseHandle(PI.hProcess);
  CloseHandle(PI.hThread);
end;

procedure StartProgram(const AAppName, AParams: string; AWait: DWORD; ACmdShow: UINT);
begin
  WinExec(Format('"%s" %s', [AAppName, AParams]), ACmdShow, AWait);
end;

function StrToIPv4(const Str: AnsiString; var addr: TInAddr; var err: Integer): Boolean;
begin
  Result:= StrToIPv4(PAnsiChar(Pointer(Str)), addr, Err);
  Inc(Err);
end;

function StrToIPv4(Str: PAnsiChar; var addr: TInAddr; var err: Integer): Boolean;
var oerr, value: Integer;
  I: Integer;
begin
  oerr:= 0;
  for I := 0 to 3 do begin
    value:= 0;
    if Str[oerr] = #0 then begin
      err:= oerr;
      Exit(False);
    end;
    while Str[oerr] <> #0 do begin
      case Str[oerr] of
        '0'..'9':
          value:= Ord(Str[oerr]) - Ord('0') + value * 10;
      else
        Break;
      end;
      Inc(oerr);
    end;
    if value > 255 then begin
      err:= oerr;
      Exit(False);
    end;
    LongRec(addr.S_addr).Bytes[I]:= value;
    if Str[oerr] <> '.' then begin
      err:= oerr;
      Exit(I = 3);
    end;
    Inc(oerr);
  end;
  err:= oerr;
  Result:= True;
end;

function StrToIPv4(const Str: string; var addr: TInAddr; var err: Integer): Boolean;
begin
  Result:= StrToIPv4(PWideChar(Pointer(Str)), addr, Err);
  Inc(Err);
end;

function StrToIPv4(Str: PWideChar; var addr: TInAddr; var err: Integer): Boolean;
var oerr, value: Integer;
  I: Integer;
begin
  oerr:= 0;
  for I := 0 to 3 do begin
    value:= 0;
    if Str[oerr] = #0 then begin
      err:= oerr;
      Exit(False);
    end;
    while Str[oerr] <> #0 do begin
      case Str[oerr] of
        '0'..'9':
          value:= Ord(Str[oerr]) - Ord('0') + value * 10;
      else
        Break;
      end;
      Inc(oerr);
    end;
    if value > 255 then begin
      err:= oerr;
      Exit(False);
    end;
    LongRec(addr.S_addr).Bytes[I]:= value;
    if Str[oerr] <> '.' then begin
      err:= oerr;
      Exit(I = 3);
    end;
    Inc(oerr);
  end;
  err:= oerr;
  Result:= True;
end;

procedure HexStrToInt(const Str: AnsiString; var Value, Err: Integer);
begin
  HexStrToInt(PAnsiChar(Pointer(Str)), Value, Err);
  Inc(Err);
end;

procedure HexStrToInt(Str: PAnsiChar; var Value, Err: Integer);
var oerr, res: Integer;
begin
  oerr:= 0;
  res:= 0;
  while Str[oerr] <> #0 do begin
    case Str[oerr] of
      '0'..'9': res:= Ord(Str[oerr]) - Ord('0') + res * 16;
      'A'..'F': res:= Ord(Str[oerr]) - Ord('A') + $A + res * 16;
      'a'..'f': res:= Ord(Str[oerr]) - Ord('a') + $A + res * 16;
    else
      Break;
    end;
    Inc(oerr);
  end;
  Value:= res;
  Err:= oerr;
end;

procedure HexStrToInt(const Str: string; var Value, Err: Integer);
begin
  HexStrToInt(PWideChar(Pointer(Str)), Value, Err);
  Inc(Err);
end;

procedure HexStrToInt(Str: PWideChar; var Value, Err: Integer);
var oerr, res: Integer;
begin
  oerr:= 0;
  res:= 0;
  while Str[oerr] <> #0 do begin
    case Str[oerr] of
      '0'..'9': res:= Ord(Str[oerr]) - Ord('0') + res * 16;
      'A'..'F': res:= Ord(Str[oerr]) - Ord('A') + $A + res * 16;
      'a'..'f': res:= Ord(Str[oerr]) - Ord('a') + $A + res * 16;
    else
      Break;
    end;
    Inc(oerr);
  end;
  Value:= res;
  Err:= oerr;
end;

function StrToIPv6(const Str: AnsiString; var addr: TIn6Addr; var err: Integer): Boolean; overload; inline;
begin
  Result:= StrToIPv6(PAnsiChar(Pointer(Str)), addr, Err);
  Inc(Err);
end;

function StrToIPv6(Str: PAnsiChar; var addr: TIn6Addr; var err: Integer): Boolean;
var oerr, I, J, C, value, verr: Integer;
    ipv4: TInAddr;
begin
  oerr:= 0;
  I:= 0;
  Result:= False;
  while I <= 7 do begin
    if Str[oerr] = ':' then begin
      Inc(oerr);
      if Str[oerr] = ':' then
        Break;
    end;
    HexStrToInt(PAnsiChar(@Str[oerr]), value, verr);
    if (verr = 0) or (value > $ffff) then begin
      err:= oerr;
      Exit;
    end;
    addr.Byte[I * 2 + 0]:= LongRec(value).Bytes[1];
    addr.Byte[I * 2 + 1]:= LongRec(value).Bytes[0];
    Inc(oerr, verr);
    Inc(I);
  end;

  J:= I;
  while J <= 7 do begin
    if Str[oerr] = ':' then begin
      Inc(oerr);
    end;
    if (J = 1) and (I = 0) and (value = $ffff) and StrToIPv4(PAnsiChar(@Str[oerr]), ipv4, verr) then begin
      addr.Word[J + 0]:= ipv4.S_un_w.s_w1;
      addr.Word[J + 1]:= ipv4.S_un_w.s_w2;
      Inc(oerr, verr);
      Inc(J, 2);
      Break;
    end;

    HexStrToInt(PAnsiChar(@Str[oerr]), value, verr);
    if value > $ffff then begin
      err:= oerr;
      Exit;
    end;
    if verr = 0 then
      Break;
    addr.Byte[J * 2 + 0]:= LongRec(value).Bytes[1];
    addr.Byte[J * 2 + 1]:= LongRec(value).Bytes[0];
    Inc(oerr, verr);
    Inc(J);
  end;

  for C := J - I - 1 downto 0 do
    addr.Word[8 - J + I + C]:= addr.Word[C + I];

  for C := I to 7 - J + I do
    addr.Word[C]:= 0;

  err:= oerr;
  Result:= True;
end;

function StrToIPv6(const Str: string; var addr: TIn6Addr; var err: Integer): Boolean; overload; inline;
begin
  Result:= StrToIPv6(PWideChar(Pointer(Str)), addr, Err);
  Inc(Err);
end;

function StrToIPv6(Str: PWideChar; var addr: TIn6Addr; var err: Integer): Boolean;
var oerr, I, J, C, value, verr: Integer;
    ipv4: TInAddr;
begin
  oerr:= 0;
  I:= 0;
  Result:= False;
  while I <= 7 do begin
    if Str[oerr] = ':' then begin
      Inc(oerr);
      if Str[oerr] = ':' then
        Break;
    end;
    HexStrToInt(PWideChar(@Str[oerr]), value, verr);
    if (verr = 0) or (value > $ffff) then begin
      err:= oerr;
      Exit;
    end;
    addr.Byte[I * 2 + 0]:= LongRec(value).Bytes[1];
    addr.Byte[I * 2 + 1]:= LongRec(value).Bytes[0];
    Inc(oerr, verr);
    Inc(I);
  end;

  J:= I;
  while J <= 7 do begin
    if Str[oerr] = ':' then begin
      Inc(oerr);
    end;
    if (J = 1) and (I = 0) and (value = $ffff) and StrToIPv4(PWideChar(@Str[oerr]), ipv4, verr) then begin
      addr.Word[J + 0]:= ipv4.S_un_w.s_w1;
      addr.Word[J + 1]:= ipv4.S_un_w.s_w2;
      Inc(oerr, verr);
      Inc(J, 2);
      Break;
    end;

    HexStrToInt(PWideChar(@Str[oerr]), value, verr);
    if value > $ffff then begin
      err:= oerr;
      Exit;
    end;
    if verr = 0 then
      Break;
    addr.Byte[J * 2 + 0]:= LongRec(value).Bytes[1];
    addr.Byte[J * 2 + 1]:= LongRec(value).Bytes[0];
    Inc(oerr, verr);
    Inc(J);
  end;

  for C := J - I - 1 downto 0 do
    addr.Word[8 - J + I + C]:= addr.Word[C + I];

  for C := I to 7 - J + I do
    addr.Word[C]:= 0;

  err:= oerr;
  Result:= True;
end;

procedure AddDropFilesExStyle(var ExStyle: DWORD);
begin
  ExStyle:= ExStyle or WS_EX_ACCEPTFILES;
end;

{ TDropFile }

constructor TDropFile.Create(ADrop: HDROP);
begin
  FDrop:= ADrop;
  FFinalizator.InitFinalizator(Finalize);
end;

procedure TDropFile.Finalize;
begin
  DragFinish(FDrop);
  FDrop:= 0;
end;

function TDropFile.GetFile(Index: Integer): string;
var i: Integer;
begin
  i:= DragQueryFile(FDrop, Index, nil, 0);
  SetLength(Result, i);
  DragQueryFile(FDrop, Index, PChar(Result), i + 1);
end;

function TDropFile.GetFileCount: Integer;
begin
  Result:= DragQueryFile(FDrop, INFINITE, nil, 0);
end;

{ TWatcher }

procedure TWatcher.BeginWatch;
begin
  if Frequency = 0 then
    FTime:= GetTickCount
  else
    QueryPerformanceCounter(FTime);
end;

function TWatcher.GetWatch: Extended;
var Temp: Int64;
begin
  if Frequency = 0 then begin
    Temp:= GetTickCount - FTime;
    if Temp < 0 then
      Inc(Temp, LongWord.MaxValue);
    Result:= Temp / 10000;
  end else begin
    QueryPerformanceCounter(Temp);
    Result:= (Temp - FTime) / Frequency;
  end;
end;

function TWatcher.GetWatch10kOfSec: Integer;
var Temp: Int64;
begin
  if Frequency = 0 then begin
    Temp:= GetTickCount - FTime;
    if Temp < 0 then
      Inc(Temp, LongWord.MaxValue);
  end else begin
    QueryPerformanceCounter(Temp);
    Temp:= (Temp - FTime) * 10000 div Frequency;
  end;
  Result:= Temp;
end;

{ TWindowsInfo }

class constructor TWindowsInfo.Create;
begin
  if TOSVersion.Platform = pfWindows then begin
    if TOSVersion.Major = 5 then begin
      case TOSVersion.Minor of
        0: FWindowsVersion:= TWindowsVersion.wvWindows2000;
        1: FWindowsVersion:= TWindowsVersion.wvWindowsXP;
      else
        FWindowsVersion:= TWindowsVersion.wvWindowsXP64;
      end;
    end else if TOSVersion.Major = 6 then begin
      case TOSVersion.Minor of
        0: FWindowsVersion:= TWindowsVersion.wvWindowsVista;
        1: FWindowsVersion:= TWindowsVersion.wvWindows7;
        2: FWindowsVersion:= TWindowsVersion.wvWindows8;
        3: FWindowsVersion:= TWindowsVersion.wvWindows8_1;
      end;
    end else if TOSVersion.Major = 10 then
      FWindowsVersion:= TWindowsVersion.wvWindows10;
  end else
    FWindowsVersion:= TWindowsVersion.wvUnknownVersion;
end;

initialization

  if not QueryPerformanceFrequency(Frequency) then
    Frequency:= 0;

end.
