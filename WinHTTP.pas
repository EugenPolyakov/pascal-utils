unit WinHTTP;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, WinAPIExtensions, RecordUtils, SysUtilsExtensions;

const
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4; // Windows 8.1 and newer
  WINHTTP_FLAG_BYPASS_PROXY_CACHE = $00000100; // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_SECURE = $00800000; // use SSL if applicable (HTTPS)
  WINHTTP_ADDREQ_FLAG_COALESCE = $40000000;

  {* flags for WinHttpOpen *}
  WINHTTP_FLAG_ASYNC                         = $10000000;

  {* Query flags *}
  WINHTTP_QUERY_MIME_VERSION                 = 0;
  WINHTTP_QUERY_CONTENT_TYPE                 = 1;
  WINHTTP_QUERY_CONTENT_TRANSFER_ENCODING    = 2;
  WINHTTP_QUERY_CONTENT_ID                   = 3;
  WINHTTP_QUERY_CONTENT_DESCRIPTION          = 4;
  WINHTTP_QUERY_CONTENT_LENGTH               = 5;
  WINHTTP_QUERY_CONTENT_LANGUAGE             = 6;
  WINHTTP_QUERY_ALLOW                        = 7;
  WINHTTP_QUERY_PUBLIC                       = 8;
  WINHTTP_QUERY_DATE                         = 9;
  WINHTTP_QUERY_EXPIRES                      = 10;
  WINHTTP_QUERY_LAST_MODIFIED                = 11;
  WINHTTP_QUERY_MESSAGE_ID                   = 12;
  WINHTTP_QUERY_URI                          = 13;
  WINHTTP_QUERY_DERIVED_FROM                 = 14;
  WINHTTP_QUERY_COST                         = 15;
  WINHTTP_QUERY_LINK                         = 16;
  WINHTTP_QUERY_PRAGMA                       = 17;
  WINHTTP_QUERY_VERSION                      = 18;
  WINHTTP_QUERY_STATUS_CODE                  = 19;
  WINHTTP_QUERY_STATUS_TEXT                  = 20;
  WINHTTP_QUERY_RAW_HEADERS                  = 21;
  WINHTTP_QUERY_RAW_HEADERS_CRLF             = 22;
  WINHTTP_QUERY_CONNECTION                   = 23;
  WINHTTP_QUERY_ACCEPT                       = 24;
  WINHTTP_QUERY_ACCEPT_CHARSET               = 25;
  WINHTTP_QUERY_ACCEPT_ENCODING              = 26;
  WINHTTP_QUERY_ACCEPT_LANGUAGE              = 27;
  WINHTTP_QUERY_AUTHORIZATION                = 28;
  WINHTTP_QUERY_CONTENT_ENCODING             = 29;
  WINHTTP_QUERY_FORWARDED                    = 30;
  WINHTTP_QUERY_FROM                         = 31;
  WINHTTP_QUERY_IF_MODIFIED_SINCE            = 32;
  WINHTTP_QUERY_LOCATION                     = 33;
  WINHTTP_QUERY_ORIG_URI                     = 34;
  WINHTTP_QUERY_REFERER                      = 35;
  WINHTTP_QUERY_RETRY_AFTER                  = 36;
  WINHTTP_QUERY_SERVER                       = 37;
  WINHTTP_QUERY_TITLE                        = 38;
  WINHTTP_QUERY_USER_AGENT                   = 39;
  WINHTTP_QUERY_WWW_AUTHENTICATE             = 40;
  WINHTTP_QUERY_PROXY_AUTHENTICATE           = 41;
  WINHTTP_QUERY_ACCEPT_RANGES                = 42;
  WINHTTP_QUERY_SET_COOKIE                   = 43;
  WINHTTP_QUERY_COOKIE                       = 44;
  WINHTTP_QUERY_REQUEST_METHOD               = 45;
  WINHTTP_QUERY_REFRESH                      = 46;
  WINHTTP_QUERY_CONTENT_DISPOSITION          = 47;
  WINHTTP_QUERY_AGE                          = 48;
  WINHTTP_QUERY_CACHE_CONTROL                = 49;
  WINHTTP_QUERY_CONTENT_BASE                 = 50;
  WINHTTP_QUERY_CONTENT_LOCATION             = 51;
  WINHTTP_QUERY_CONTENT_MD5                  = 52;
  WINHTTP_QUERY_CONTENT_RANGE                = 53;
  WINHTTP_QUERY_ETAG                         = 54;
  WINHTTP_QUERY_HOST                         = 55;
  WINHTTP_QUERY_IF_MATCH                     = 56;
  WINHTTP_QUERY_IF_NONE_MATCH                = 57;
  WINHTTP_QUERY_IF_RANGE                     = 58;
  WINHTTP_QUERY_IF_UNMODIFIED_SINCE          = 59;
  WINHTTP_QUERY_MAX_FORWARDS                 = 60;
  WINHTTP_QUERY_PROXY_AUTHORIZATION          = 61;
  WINHTTP_QUERY_RANGE                        = 62;
  WINHTTP_QUERY_TRANSFER_ENCODING            = 63;
  WINHTTP_QUERY_UPGRADE                      = 64;
  WINHTTP_QUERY_VARY                         = 65;
  WINHTTP_QUERY_VIA                          = 66;
  WINHTTP_QUERY_WARNING                      = 67;
  WINHTTP_QUERY_EXPECT                       = 68;
  WINHTTP_QUERY_PROXY_CONNECTION             = 69;
  WINHTTP_QUERY_UNLESS_MODIFIED_SINCE        = 70;
  WINHTTP_QUERY_PROXY_SUPPORT                = 75;
  WINHTTP_QUERY_AUTHENTICATION_INFO          = 76;
  WINHTTP_QUERY_PASSPORT_URLS                = 77;
  WINHTTP_QUERY_PASSPORT_CONFIG              = 78;
  WINHTTP_QUERY_MAX                          = 78;
  WINHTTP_QUERY_CUSTOM                       = 65535;
  WINHTTP_QUERY_FLAG_REQUEST_HEADERS         = $80000000;
  WINHTTP_QUERY_FLAG_SYSTEMTIME              = $40000000;
  WINHTTP_QUERY_FLAG_NUMBER                  = $20000000;

  // taken from http://www.tek-tips.com/faqs.cfm?fid=7493
  // status manifests for WinHttp status callback
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = $00000001;
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = $00000002;
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = $00000004;
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = $00000008;
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = $00000010;
    WINHTTP_CALLBACK_STATUS_REQUEST_SENT = $00000020;
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = $00000040;
    WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = $00000080;
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = $00000100;
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = $00000200;
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = $00000400;
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = $00000800;
    WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = $00001000;
    WINHTTP_CALLBACK_STATUS_REDIRECT = $00004000;
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = $00008000;
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = $00010000;
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = $00020000;
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = $00040000;
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE = $00080000;
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = $00100000;
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = $00200000;
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = $00400000;

    WINHTTP_CALLBACK_FLAG_RESOLVE_NAME =
     (WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED);
    WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER =
     (WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or
      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER);
    WINHTTP_CALLBACK_FLAG_SEND_REQUEST =
     (WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or
      WINHTTP_CALLBACK_STATUS_REQUEST_SENT);
    WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE =
     (WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or
      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED);
    WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION =
     (WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or
      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED);
    WINHTTP_CALLBACK_FLAG_HANDLES =
     (WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or
      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING);
    WINHTTP_CALLBACK_FLAG_DETECTING_PROXY = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
    WINHTTP_CALLBACK_FLAG_REDIRECT = WINHTTP_CALLBACK_STATUS_REDIRECT;
    WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE =  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
    WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
    WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
    WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_READ_COMPLETE = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
    WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
    WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;

    WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS =
        (WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
       or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
       or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
       or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR);
    WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = $ffffffff;

    WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
    WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
    // tls 1.1 & 1.2 const from here:
    // https://github.com/nihon-tc/Rtest/blob/master/header/Microsoft%20SDKs/Windows/v7.0A/Include/winhttp.h
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;

   // Sets an unsigned long integer value that specifies which secure protocols are acceptable.
   // By default only SSL3 and TLS1 are enabled in Windows 7 and Windows 8.
   // By default only SSL3, TLS1.0, TLS1.1, and TLS1.2 are enabled in Windows 8.1 and Windows 10.
   WINHTTP_OPTION_SECURE_PROTOCOLS = 84;
   // Instructs the stack to start a WebSocket handshake process with WinHttpSendRequest.
   // This option takes no parameters.
   WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;

   // if the following value is returned by WinHttpSetStatusCallback, then
   // probably an invalid (non-code) address was supplied for the callback
   WINHTTP_INVALID_STATUS_CALLBACK = Pointer(-1);

   WINHTTP_OPTION_DISABLE_FEATURE = 63;
   // values for WINHTTP_OPTION_DISABLE_FEATURE
   WINHTTP_DISABLE_COOKIES = $00000001;
   WINHTTP_DISABLE_REDIRECTS = $00000002;
   WINHTTP_DISABLE_AUTHENTICATION = $00000004;
   WINHTTP_DISABLE_KEEP_ALIVE = $00000008;

   WINHTTP_OPTION_ENABLE_FEATURE = 79;
   // values for WINHTTP_OPTION_ENABLE_FEATURE
   WINHTTP_ENABLE_SSL_REVOCATION = $00000001;
   WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION = $00000002;

type
  HINTERNET = THandle;

  WINHTTP_STATUS_CALLBACK = procedure(hInternet: HINTERNET; dwContext: Pointer;
    dwInternetStatus: DWORD; lpvStatusInformation: pointer; dwStatusInformationLength: DWORD); stdcall;

  PWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;

  TURL = record
  public
    /// if the server is accessible via something else than http:// or https://
    // - e.g. 'ws' or 'wss' for ws:// or wss://
    Scheme: string;
    /// the server name
    // - e.g. 'www.somewebsite.com'
    Server: string;
    /// the server port
    // - e.g. '80'
    Port: string;
    /// the resource address
    // - e.g. '/category/name/10?param=1'
    Address: string;
    /// fill the members from a supplied URI
    function From(AURL: string; const DefaultPort: string=''): boolean;
    /// compute the whole normalized URI
    function FullURL: string;
    /// the server port, as integer value
    function PortInt: Word;
    /// compute the root resource Address, without any URI-encoded parameter
    // - e.g. '/category/name/10'
    function Root: string;
    /// reset all stored information
    procedure Clear;
  end;

  THTTPClient = class;
  THTTPAsyncContext = class;

  EHTTPException = class (Exception);
  EHTTPWrapperException = class (EHTTPException)
  private
    FContext: THTTPAsyncContext;
  public
    destructor Destroy; override;
    property Context: THTTPAsyncContext read FContext;
    constructor Create(const AMessage: string; AContext: THTTPAsyncContext);
  end;

  EHTTPAlreadyRunning = class (EHTTPException);

  THTTPConnectOptions = record
    ResolveTimeout: DWORD;
    ConnectTimeout: DWORD;
    SendTimeout: DWORD;
    ReceiveTimeout: DWORD;
    UseHttps: Boolean;
  end;


  THTTPRequest = class
  private
    FKeepAlive: Boolean;
    FUseSSL: Boolean;
    FHeaders: TListRecord<string>;
    FTotalDataLength: Integer;
    procedure SetKeepAlive(const Value: Boolean);
  protected
    FMethod: string;
  public
    function GetHeaders: string;
    procedure AddHeader(const Header: string);
    function GetNextDataChunk(var Data; ALength: Integer): Integer; virtual;
    function HaveDataToSend: Boolean; virtual; abstract;
    //if not zero then create Content-length: xx header
    property TotalDataLength: Integer read FTotalDataLength write FTotalDataLength;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive;
    property Method: string read FMethod;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
  end;

  THTTPGetRequest = class(THTTPRequest)
  public
    function HaveDataToSend: Boolean; override;
    constructor Create;
  end;

  THTTPPostRequest = class(THTTPRequest)
  private
  protected
  public
    constructor Create;
  end;

  THTTPPostRequestConstBuffer = class(THTTPPostRequest)
  private
    FBuffer: TArray<Byte>;
    FOffset: Integer;
  protected
  public
    function GetNextDataChunk(var Data; ALength: Integer): Integer; override;
    function HaveDataToSend: Boolean; override;
    constructor Create(const ABuffer: TArray<Byte>); overload;
    constructor Create(const ABuffer; ALength: Integer); overload;
  end;

  THTTPPostRequestStream = class(THTTPPostRequest)
  private
    FStream: TStream;
    FHasData: Boolean;
  protected
  public
    function GetNextDataChunk(var Data; ALength: Integer): Integer; override;
    function HaveDataToSend: Boolean; override;
    property InputStream: TStream read FStream;
    constructor Create(AStream: TStream);
  end;

  THTTPResponse = class
  private
    FContext: THTTPAsyncContext;
    FDataSize: Int64;
    function GetIntegerHeaderByName(const AName: string): LongWord;
  protected
    FHTTPStatus: Integer;
    property Context: THTTPAsyncContext read FContext;
    function GetHeader(ARequest: HINTERNET; Info: DWORD): string;
    function GetHeader32(ARequest: HINTERNET; Info: DWORD; DefaultValue: DWORD = DWORD(-1)): DWORD;
    procedure WriteData(const Buffer; ALength: Integer); virtual;
    //procedure GetRequest(AClient: THTTPClient; ARequest: HINTERNET); virtual; abstract;
    procedure EndResponse(AReaded: Integer); virtual;
    function IsNeedRead(ARequest: HINTERNET): Boolean; virtual;
  public
    property HTTPStatus: Integer read FHTTPStatus;
    property DataSize: Int64 read FDataSize;
    property HeadersInteger[const AName: string]: LongWord read GetIntegerHeaderByName;
  end;

  THTTPCustomStreamResponse = class (THTTPResponse)
  private
    FOutStream: TStream;
    FStreamOwner: Boolean;
  protected
    procedure WriteData(const Buffer; ALength: Integer); override;
  public
    destructor Destroy; override;
    property OutStream: TStream read FOutStream;
    constructor Create(AOutStream: TStream; AOwnStream: Boolean);
  end;

  THTTPStringResponse = class (THTTPCustomStreamResponse)
  private
    FResult: string;
    FEncoding: TEncoding;
    FEncodingOwner: Boolean;
  protected
    function IsNeedRead(ARequest: HINTERNET): Boolean; override;
    procedure EndResponse(AReaded: Integer); override;
  public
    property Result: string read FResult;
    constructor Create;
    destructor Destroy; override;
  end;

  THTTPNotifyLoadResponse = class;
  TNotifyBeginLoad = procedure (AResponse: THTTPNotifyLoadResponse; FullLength: LongWord) of object;
  TNotifyEndLoad = procedure (AResponse: THTTPNotifyLoadResponse; FinalLength: LongWord) of object;
  TNotifyLoading = procedure (AResponse: THTTPNotifyLoadResponse; const Data; BufferLen: LongWord) of object;

  THTTPNotifyLoadResponse = class (THTTPResponse)
  private
    FNotifyBeginLoad: TNotifyBeginLoad;
    FNotifyEndLoad: TNotifyEndLoad;
    FNotifyLoading: TNotifyLoading;
  protected
    function IsNeedRead(ARequest: HINTERNET): Boolean; override;
    procedure WriteData(const Buffer; ALength: Integer); override;
    procedure EndResponse(AReaded: Integer); override;
  public
    constructor Create;
    property NotifyBeginLoad: TNotifyBeginLoad read FNotifyBeginLoad write
        FNotifyBeginLoad;
    property NotifyEndLoad: TNotifyEndLoad read FNotifyEndLoad write FNotifyEndLoad;
    property NotifyLoading: TNotifyLoading read FNotifyLoading write FNotifyLoading;
    property Context;
  end;

  TSilentTerminator = function : Boolean of object;

  THTTPStreamResponse = class (THTTPCustomStreamResponse)
  private
    FIsTerminated: TSilentTerminator;
    FOnlySuccess: Boolean;
  protected
    function IsNeedRead(ARequest: HINTERNET): Boolean; override;
    procedure WriteData(const Buffer; ALength: Integer); override;
  public
    constructor Create(AOutStream: TStream; AOwnStream: Boolean = True; AIsTerminated: TSilentTerminator = nil; AOnlySuccess: Boolean = True);
  end;

  THTTPAsyncContextEndLoading = procedure (const AContext: THTTPAsyncContext) of object;
  //THTTPAsyncContextEndLoading = TAction<THTTPAsyncContext>;
  //not used because XE5 can't call procedure Run(A: THTTPAsyncContextEndLoading)

  THTTPAsyncContext = class
  private
    FProcessedDataLength: Integer;
    FBufferData: array [0..4095] of Byte;
    FRequest: THTTPRequest;
    FResponse: THTTPResponse;
    FClient: THTTPClient;
    FHandle: HINTERNET;
    FConnection: HINTERNET;
    //FSendedSize: Int64;
    FReadedSize: Int64;
    FLastReadedSize: LongWord;
    FIsClosed: Boolean;
    FDisposed: Boolean;
    FNeedDestroyClient, FNeedDestroyAll, FDisposeAfterLoad: Boolean;
    FOnEndLoad: THTTPAsyncContextEndLoading;
    FStarted: Integer;
    procedure RaiseError;
    procedure SetOnEndLoad(const Value: THTTPAsyncContextEndLoading);
  protected
    procedure DoNotifyEndLoad;
  public
    property Client: THTTPClient read FClient;
    property Request: THTTPRequest read FRequest;
    property Response: THTTPResponse read FResponse;
    property Handle: HINTERNET read FHandle;
    property Connection: HINTERNET read FConnection;
    function GetNextDataChunk(var Data; ALength: Integer): Integer; inline;
    procedure ReadNextChunk;
    procedure Run; overload;
    procedure Run(AOnEndLoad: THTTPAsyncContextEndLoading; ADisposeAfterLoad: Boolean = True); overload;
    procedure Callback(AInternet: HINTERNET; Status: DWORD; StatusInformation: Pointer; StatusInformationLength: DWORD);
    property IsClosed: Boolean read FIsClosed;
    constructor Create(AClient: THTTPClient; ARequest: THTTPRequest;
        AResponse: THTTPResponse; AHandle, AConnection: HINTERNET);
    property NeedDestroyClient: Boolean read FNeedDestroyClient write FNeedDestroyClient;
    property NeedDestroyAll: Boolean read FNeedDestroyAll write FNeedDestroyAll;
    property DisposeAfterLoad: Boolean read FDisposeAfterLoad write FDisposeAfterLoad;
    property OnEndLoad: THTTPAsyncContextEndLoading read FOnEndLoad write SetOnEndLoad;
    destructor Destroy; override;
    procedure Dispose;
  end;

  THTTPSender = record
    Session: HINTERNET;
    Connection: HINTERNET;
    Request: THTTPRequest;
    Response: THTTPResponse;

  end;
  PHTTPSender = ^THTTPSender;

  THTTPConnection = record
  private
    FConnection: HINTERNET;
  public
    property Connection: HINTERNET read FConnection;
  end;

  THTTPClient = class
  private
    FIsAsynchronous: Boolean;
    FSession: HINTERNET;
  protected
    procedure AddHeader(ARequest: HINTERNET; const Header: string);
    procedure CloseSession;
    procedure SendRequestData;
  public
    ConnectTimeout: DWORD;
    ReceiveTimeout: DWORD;
    ResolveTimeout: DWORD;
    SendTimeout: DWORD;
    constructor Create; overload;
    constructor Create(const AUserAgent, AProxyName, AProxyByPass: string; AAsync: Boolean); overload;
    destructor Destroy; override;
    class function Get(const AURL: string): string;
    class function GetAsyncString(const AURL: string): THTTPAsyncContext;
    procedure InitSession(const AUserAgent, AProxyName, AProxyByPass: string; AAsync: Boolean);
    function Send(ARequest: THTTPRequest; AResponse: THTTPResponse; const AURL: TURL): THTTPAsyncContext;
    property IsAsynchronous: Boolean read FIsAsynchronous;
  end;

  /// types of WebSocket buffers for winhttp.dll
  // it is the different thing than WEB_SOCKET_BUFFER_TYPE for httpapi.dll
  WINHTTP_WEB_SOCKET_BUFFER_TYPE = ULONG;

  /// direct late-binding access to the WinHTTP API
  // - note: WebSocket* API calls require Windows 8 and later
  TWinHTTPBinding = packed record
    /// access to the winhttp.dll loaded library
    LibraryHandle: HMODULE;
    /// depends on the published .dll functions
    WebSocketEnabled: Boolean;
    /// Initializes an application's use of the WinHTTP functions.
    Open: function(pwszUserAgent: PWideChar; dwAccessType: DWORD;
      pwszProxyName, pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall;
    /// Sets up a callback function that WinHTTP can call as progress is made during an operation.
    SetStatusCallback: function(hSession: HINTERNET; lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
      dwNotificationFlags: DWORD; dwReserved: PDWORD): WINHTTP_STATUS_CALLBACK; stdcall;
    /// Specifies the initial target server of an HTTP request.
    Connect: function(hSession: HINTERNET; pswzServerName: PWideChar;
      nServerPort: WORD; dwReserved: DWORD): HINTERNET; stdcall;
    /// Creates an HTTP request handle.
    OpenRequest: function(hConnect: HINTERNET; pwszVerb: PWideChar;
      pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
      ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;
    /// Closes a single HINTERNET handle.
    CloseHandle: function(hInternet: HINTERNET): BOOL; stdcall;
    /// Adds one or more HTTP request headers to the HTTP request handle.
    AddRequestHeaders: function(hRequest: HINTERNET; pwszHeaders: PWideChar; dwHeadersLength: DWORD;
      dwModifiers: DWORD): BOOL; stdcall;
    /// Sends the specified request to the HTTP server.
    SendRequest: function(hRequest: HINTERNET; pwszHeaders: PWideChar;
      dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD; dwTotalLength: DWORD;
      dwContext: Pointer): BOOL; stdcall;
    /// Ends an HTTP request that is initiated by WinHttpSendRequest.
    ReceiveResponse: function(hRequest: HINTERNET;
      lpReserved: Pointer): BOOL; stdcall;
    /// Retrieves header information associated with an HTTP request.
    QueryHeaders: function(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: PWideChar;
      lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;
    /// Reads data from a handle opened by the WinHttpOpenRequest function.
    ReadData: function(hRequest: HINTERNET; lpBuffer: Pointer;
      dwNumberOfBytesToRead: DWORD; lpdwNumberOfBytesRead: PDWORD): BOOL; stdcall;
    WriteData : function(hRequest: HINTERNET; lpBuffer: Pointer;
      dwNumberOfBytesToWrite: DWORD; lpdwNumberOfBytesWritten: PDWORD): BOOL; stdcall;
    /// Sets the various time-outs that are involved with HTTP transactions.
    SetTimeouts: function(hInternet: HINTERNET; dwResolveTimeout: DWORD;
      dwConnectTimeout: DWORD; dwSendTimeout: DWORD; dwReceiveTimeout: DWORD): BOOL; stdcall;
    /// Sets an Internet option.
    SetOption: function(hInternet: HINTERNET; dwOption: DWORD;
      lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
    /// Passes the required authorization credentials to the server.
    SetCredentials: function(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD;
      pwszUserName: PWideChar; pwszPassword: PWideChar; pAuthParams: Pointer) : BOOL; stdcall;
    /// Completes a WebSocket handshake started by WinHttpSendRequest.
    WebSocketCompleteUpgrade: function(hRequest: HINTERNET;
      lpReserved: Pointer): HINTERNET; stdcall;
    /// Closes a WebSocket connection.
    WebSocketClose: function(hWebSocket: HINTERNET; usStatus: Word;
      pvReason: Pointer; dwReasonLength: DWORD): DWORD; stdcall;
    /// Retrieves the close status sent by a server
    WebSocketQueryCloseStatus: function(hWebSocket: HINTERNET; out usStatus: Word;
      pvReason: Pointer; dwReasonLength: DWORD; out dwReasonLengthConsumed: DWORD): DWORD; stdcall;
    /// Sends data over a WebSocket connection.
    WebSocketSend: function(hWebSocket: HINTERNET; eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
      pvBuffer: Pointer; dwBufferLength: DWORD): DWORD; stdcall;
    /// Receives data from a WebSocket connection.
    WebSocketReceive: function(hWebSocket: HINTERNET; pvBuffer: Pointer; dwBufferLength: DWORD;
      out dwBytesRead: DWORD; out eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): DWORD; stdcall;
  end;

procedure RequestCallback(hInternet: HINTERNET; Context: Pointer; dwInternetStatus: DWORD;
    lpvStatusInformation: Pointer; dwStatusInformationLength: DWORD); stdcall;

implementation

{$IFDEF USE_VCL}
uses
  Vcl.Forms;
{$ENDIF}

{ TWinHTTP }

var
  WinHttpAPI: TWinHTTPBinding;

type
  TWinHttpAPIs = (hOpen, hSetStatusCallback, hConnect,
    hOpenRequest, hCloseHandle, hAddRequestHeaders,
    hSendRequest, hReceiveResponse, hQueryHeaders,
    hReadData, hWriteData, hSetTimeouts, hSetOption, hSetCredentials,
    hWebSocketCompleteUpgrade, hWebSocketClose, hWebSocketQueryCloseStatus,
    hWebSocketSend, hWebSocketReceive);
const
  hWebSocketApiFirst = hWebSocketCompleteUpgrade;

const
  WinHttpNames: array[TWinHttpAPIs] of PChar = (
    'WinHttpOpen', 'WinHttpSetStatusCallback', 'WinHttpConnect',
    'WinHttpOpenRequest', 'WinHttpCloseHandle', 'WinHttpAddRequestHeaders',
    'WinHttpSendRequest', 'WinHttpReceiveResponse', 'WinHttpQueryHeaders',
    'WinHttpReadData', 'WinHttpWriteData', 'WinHttpSetTimeouts', 'WinHttpSetOption', 'WinHttpSetCredentials',
    'WinHttpWebSocketCompleteUpgrade', 'WinHttpWebSocketClose', 'WinHttpWebSocketQueryCloseStatus',
    'WinHttpWebSocketSend', 'WinHttpWebSocketReceive');

procedure WinHttpAPIInitialize;
var api: TWinHttpAPIs;
    P: PPointer;
begin
  if WinHttpAPI.LibraryHandle <> 0 then
    exit; // already loaded
  WinHttpAPI.LibraryHandle := SafeLoadLibrary(winhttpdll);
  WinHttpAPI.WebSocketEnabled := true; // WebSocketEnabled if all functions are available
  if WinHttpAPI.LibraryHandle=0 then
    raise Exception.CreateFmt('Unable to load library %s',[winhttpdll]);
  P := @@WinHttpAPI.Open;
  for api := low(api) to high(api) do begin
    P^ := GetProcAddress(WinHttpAPI.LibraryHandle,WinHttpNames[api]);
    if P^=nil then
      if api<hWebSocketApiFirst then begin
        FreeLibrary(WinHttpAPI.LibraryHandle);
        WinHttpAPI.LibraryHandle := 0;
        raise Exception.CreateFmt('Unable to find %s() in %s',[WinHttpNames[api], winhttpdll]);
      end else
        WinHttpAPI.WebSocketEnabled := false; // e.g. version is lower than Windows 8
    inc(P);
  end;
  {if WinHttpAPI.WebSocketEnabled then
    WebSocketApiInitialize else
    WebSocketAPI.WebSocketEnabled := false;}
end;

procedure RequestCallback(hInternet: HINTERNET; Context: Pointer; dwInternetStatus: DWORD;
    lpvStatusInformation: Pointer; dwStatusInformationLength: DWORD); stdcall;
{$IFDEF USE_VCL}
var ex: Pointer;
{$ENDIF}
begin
  try
    if Context <> nil then
      THTTPAsyncContext(Context).Callback(hInternet, dwInternetStatus, lpvStatusInformation, dwStatusInformationLength);
  except
{$IFDEF USE_VCL}
    try
      Exception.RaiseOuterException(EHTTPWrapperException.Create('Something was wrong!', THTTPAsyncContext(Context)));
    except
      ex:= AcquireExceptionObject;
      TThread.Queue(nil, procedure begin
        raise TObject(ex);
      end);
    end;
{$ELSE}
    if THTTPAsyncContext(Context).DisposeAfterLoad then
      THTTPAsyncContext(Context).Dispose;
{$ENDIF}
  end;
end;

{ TURL }

const
  DEFAULT_PORT: array[boolean] of string = ('80','443');

procedure TURL.Clear;
begin
  Finalize(self);
end;

function TURL.From(AURL: string; const DefaultPort: string): boolean;
var i, l: Integer;
    Https: Boolean;
begin
//TO DO user/psw
  Clear;
  Result:= False;
  AURL:= Trim(AURL);
  if AURL = '' then
    exit;
  i:= Low(string);
  while (AURL[i] <> ':') and (AURL[i] <> #0) do Inc(i);
  if (AURL[i] <> #0) and (AURL[i + 1] = '/') and (AURL[i + 2] = '/') then begin
    Scheme:= LowerCase(Copy(AURL, Low(string), i - Low(string)));
    Https:= Scheme = 'https';
    Inc(i, 3);
  end else begin
    i:= Low(string);
    https:= false;
  end;
  l:= i;
  while (AURL[i] <> ':') and (AURL[i] <> '/') and (AURL[i] <> #0) do Inc(i);
  Server:= Copy(AURL, l, i - l);
  if AURL[i] = ':' then begin
    Inc(i);
    l:= i;
    while (AURL[i] <> '/') and (AURL[i] <> #0) do Inc(i);
    Port:= Copy(AURL, l, i - l);
    if AURL[i]<>#0 then //'/'
      Inc(i);
  end else
    if DefaultPort<>'' then
      Port := DefaultPort else
      Port := DEFAULT_PORT[Https];
  Address:= Copy(AURL, i);
  if Address[Low(string)] <> '/' then
    Address:= '/' + Address;
  if Server <> '' then
    Result:= True;
end;

function TURL.FullURL: string;
const Prefix: array[boolean] of string = ('http://','https://');
var https: Boolean;
begin
  https:= Scheme = 'https';
  if (Port = '') or (Port = '0') or (Port = DEFAULT_PORT[Https]) then
    Result:= Prefix[Https]+Server+Address else
    Result:= Prefix[Https]+Server+':'+Port+Address;
end;

function TURL.PortInt: Word;
var err: Integer;
begin
  Val(Port, Result, err);
  if err <> 0 then
    result:= 0;
end;

function TURL.Root: string;
var i: integer;
begin
  i := Pos('?',Address);
  if i=0 then
    Root:= Address
  else
    Root:= Copy(Address, 1, i-1);
end;

constructor THTTPClient.Create;
begin
  ResolveTimeout:= 60000;
  ReceiveTimeout:= 60000;
  ConnectTimeout:= 60000;
  SendTimeout:= 60000;
end;

constructor THTTPClient.Create(const AUserAgent, AProxyName, AProxyByPass: string;
  AAsync: Boolean);
begin
  Create;
  InitSession(AUserAgent, AProxyName, AProxyByPass, AAsync);
end;

destructor THTTPClient.Destroy;
begin
  CloseSession;
  inherited;
end;

{ THTTPClient }

procedure THTTPClient.AddHeader(ARequest: HINTERNET; const Header: string);
begin
  if (Header <> '') and
    not WinHttpAPI.AddRequestHeaders(ARequest, Pointer(Header), length(Header),
      WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastOsError;
end;

procedure THTTPClient.CloseSession;
begin
  if FSession <> 0 then begin
    WinHttpAPI.CloseHandle(FSession);
    FSession:= 0;
  end;
end;

class function THTTPClient.Get(const AURL: string): string;
var URL: TURL;
    client: THTTPClient;
    req: THTTPGetRequest;
    res: THTTPStringResponse;
begin
  if URL.From(AURL) then begin
    req:= nil;
    res:= nil;
    client:= THTTPClient.Create('Mozilla/5.0 (Windows NT 6.3; Win64; x64; Trident/7.0; rv:11.0) like Gecko', '', '', False);
    try
      req:= THTTPGetRequest.Create;
      req.UseSSL:= URL.Scheme = 'https';
      res:= THTTPStringResponse.Create;
      client.Send(req, res, URL);
      if res.HTTPStatus <> 200 then
        raise Exception.CreateFmt('Wrong status: %d', [res.HTTPStatus]);
      Result:= res.Result;
    finally
      client.Free;
      req.Free;
      res.Free;
    end;
  end;
end;

class function THTTPClient.GetAsyncString(const AURL: string): THTTPAsyncContext;
var URL: TURL;
    client: THTTPClient;
    req: THTTPGetRequest;
    res: THTTPStringResponse;
begin
  Result:= nil;
  if URL.From(AURL) then begin
    req:= nil;
    res:= nil;
    client:= THTTPClient.Create('Mozilla/5.0 (Windows NT 6.3; Win64; x64; Trident/7.0; rv:11.0) like Gecko', '', '', True);
    try
      req:= THTTPGetRequest.Create;
      req.UseSSL:= URL.Scheme = 'https';
      res:= THTTPStringResponse.Create;
      Result:= client.Send(req, res, URL);
      Result.NeedDestroyClient:= True;
      Result.NeedDestroyAll:= True;
    finally
      if Result = nil then begin
        client.Free;
        req.Free;
        res.Free;
      end;
    end;
  end;
end;

procedure THTTPClient.InitSession(const AUserAgent, AProxyName,
  AProxyByPass: string; AAsync: Boolean);
var OpenType: DWORD;
    Flags: DWORD;
    protocols: DWORD;
begin
  CloseSession;
  if AProxyName = '' then
    if TWindowsInfo.WindowsVersion >= TWindowsVersion.wvWindows8_1 then
      OpenType := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY
    else
      OpenType := WINHTTP_ACCESS_TYPE_DEFAULT_PROXY
  else
    OpenType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  if AAsync then
    Flags:= WINHTTP_FLAG_ASYNC
  else
    Flags:= 0;
  FIsAsynchronous:= AAsync;
  FSession := WinHttpAPI.Open(pointer(AUserAgent), OpenType,
    pointer(AProxyName), pointer(AProxyByPass), Flags);
  if FSession = 0 then
    RaiseLastOsError;

  try
    if not WinHttpAPI.SetTimeouts(FSession, ResolveTimeout,
       ConnectTimeout, SendTimeout, ReceiveTimeout) then
      RaiseLastOsError;

    protocols := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
     // Windows 7 and newer support TLS 1.1 & 1.2
    if TWindowsInfo.WindowsVersion >= TWindowsVersion.wvWindows7 then
      protocols := protocols or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
    if not WinHttpAPI.SetOption(FSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
       @protocols, SizeOf(protocols)) then
      RaiseLastOsError;
  except
    CloseSession;
  end;
end;

function THTTPClient.Send(ARequest: THTTPRequest; AResponse: THTTPResponse; const AURL: TURL): THTTPAsyncContext;
const ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
var Flags: DWORD;
    Callback: WINHTTP_STATUS_CALLBACK;
    CallbackRes: NativeInt absolute Callback;
    Connection, Request: HINTERNET;
    Context: THTTPAsyncContext;
    CallbackResult: WINHTTP_STATUS_CALLBACK;
begin
  Connection := WinHttpAPI.Connect(FSession, pointer(AURL.Server), AURL.PortInt, 0);
  if Connection = 0 then
    RaiseLastOsError;

  Context:= nil;
  Result:= nil;
  Request:= 0;
  try
    //Open request
    Flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
    if ARequest.UseSSL then
      Flags := Flags or WINHTTP_FLAG_SECURE;
    Request := WinHttpAPI.OpenRequest(Connection, Pointer(ARequest.Method),
      Pointer(AURL.Address), nil, nil, @ALL_ACCEPT, Flags);
    if Request = 0 then
      RaiseLastOsError;

    Context:= THTTPAsyncContext.Create(Self, ARequest, AResponse, Request, Connection);

    if not ARequest.KeepAlive then begin
      Flags := WINHTTP_DISABLE_KEEP_ALIVE;
      if not WinHttpAPI.SetOption(Request, WINHTTP_OPTION_DISABLE_FEATURE, @Flags, sizeOf(Flags)) then
        RaiseLastOsError;
    end;

    if FIsAsynchronous then begin
      Result:= Context;
      CallbackResult := WinHttpAPI.SetStatusCallback(Request, RequestCallback,
         WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, nil);
      if @CallbackResult = WINHTTP_INVALID_STATUS_CALLBACK then
        RaiseLastOsError;
    end;

    if Result = nil then begin
      Context.Run;
      //Send Request Data
      while ARequest.HaveDataToSend do
        Context.Callback(Context.Handle, WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE, nil, 0);

      Context.Callback(Context.Handle, WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE, nil, 0);

      Context.Callback(Context.Handle, WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE, nil, 0);

      while not Context.IsClosed do
        Context.ReadNextChunk;
    end;
  finally
    if Result = nil then
      if Context = nil then begin
        if Request <> 0 then
          WinHttpAPI.CloseHandle(Request);

        if Connection <> 0 then
          WinHttpAPI.CloseHandle(Connection);
      end else
        Context.Destroy;
  end;
end;

procedure THTTPClient.SendRequestData;
begin

end;

procedure THTTPRequest.AddHeader(const Header: string);
var p: Integer;
  I: Integer;
begin
  p:= Pos(':', Header);
  for I := 0 to FHeaders.Count - 1 do
    if StrLComp(PChar(Pointer(FHeaders[i])), PChar(Pointer(Header)), p) = 0 then begin
      FHeaders[i]:= Header;
      Exit;
    end;
  FHeaders.Add(Header);
end;

function THTTPRequest.GetHeaders: string;
begin
  if FHeaders.Count > 0 then
    Result:= string.Join(#13#10, FHeaders.List, 0, FHeaders.Count)
  else
    Result:= '';
end;

function THTTPRequest.GetNextDataChunk(var Data; ALength: Integer): Integer;
begin
  Result:= 0;
end;

procedure THTTPRequest.SetKeepAlive(const Value: Boolean);
begin
  FKeepAlive := Value;
end;

{ THTTPGetRequest }

constructor THTTPGetRequest.Create;
begin
  FMethod:= 'GET';
end;

function THTTPGetRequest.HaveDataToSend: Boolean;
begin
  Result:= False;
end;

{ THTTPStringResponse }

constructor THTTPStringResponse.Create;
begin
  inherited Create(TBytesStream.Create(), True);
end;

destructor THTTPStringResponse.Destroy;
begin
  if FEncodingOwner then
    FEncoding.Free;
  inherited;
end;

procedure THTTPStringResponse.EndResponse(AReaded: Integer);
begin
  if FEncoding = nil then
    TEncoding.GetBufferEncoding(TBytesStream(OutStream).Bytes, FEncoding);

  FResult:= FEncoding.GetString(TBytesStream(OutStream).Bytes, 0, OutStream.Size);
end;

function THTTPStringResponse.IsNeedRead(ARequest: HINTERNET): Boolean;
var contentType: string;
    p, e: Integer;
begin
  Result:= inherited IsNeedRead(ARequest);
  if Result then begin
    contentType:= GetHeader(ARequest, WINHTTP_QUERY_CONTENT_TYPE);
    if contentType <> 'application/octet-stream' then begin
      p:= Pos('; charset=', contentType);
      if p > 0 then begin
        e:= Pos(';', contentType, p + 10);
        if e > 0 then
          contentType:= Copy(contentType, p + 10, e - p - 10 - 1)
        else
          contentType:= Copy(contentType, p + 10);
        FEncoding:= TEncoding.GetEncoding(Trim(contentType));
      end;
      if FEncoding = nil then
        FEncoding:= TEncoding.GetEncoding('iso-8859-1');
      FEncodingOwner:= True;
    end;
  end;
end;

{ THTTPResponse }

procedure THTTPResponse.EndResponse(AReaded: Integer);
begin

end;

function THTTPResponse.GetHeader(ARequest: HINTERNET; Info: DWORD): string;
var Size, Index: DWORD;
begin
  Result:= '';
  Index:= 0;
  if not WinHttpAPI.QueryHeaders(ARequest, Info, nil, nil, Size, Index) and
     (GetLastError = ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(Result, Size shr 1 - 1);
    if not WinHttpAPI.QueryHeaders(ARequest, Info, nil, Pointer(Result), Size, Index) then
      Result:= '';
  end;
end;

function THTTPResponse.GetHeader32(ARequest: HINTERNET; Info, DefaultValue: DWORD): DWORD;
var Size, Index: DWORD;
begin
  Size:= 4;
  Index:= 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpAPI.QueryHeaders(ARequest, Info, nil, @Result, Size, Index) then
    Result:= DefaultValue;
end;

function THTTPResponse.GetIntegerHeaderByName(const AName: string): LongWord;
var Size, Index: DWORD;
begin
  Size:= 4;
  Index:= 0;
  if not WinHttpAPI.QueryHeaders(Context.Handle, WINHTTP_QUERY_FLAG_NUMBER or WINHTTP_QUERY_CUSTOM, Pointer(AName), @Result, Size, Index) then
    Result:= 0;
end;

function THTTPResponse.IsNeedRead(ARequest: HINTERNET): Boolean;
begin
  FHTTPStatus:= GetHeader32(ARequest, WINHTTP_QUERY_STATUS_CODE);
  Result:= FHTTPStatus = 200;
end;

procedure THTTPResponse.WriteData(const Buffer; ALength: Integer);
begin
  Inc(FDataSize, ALength);
end;

{ THTTPNotifyLoadResponse }

constructor THTTPNotifyLoadResponse.Create;
begin
  inherited;
end;

procedure THTTPNotifyLoadResponse.EndResponse(AReaded: Integer);
begin
  inherited;
  if Assigned(FNotifyEndLoad) then
    NotifyEndLoad(Self, AReaded);
end;

function THTTPNotifyLoadResponse.IsNeedRead(ARequest: HINTERNET): Boolean;
var contentLength: DWORD;
begin
  Result:= inherited IsNeedRead(ARequest);
  if Result and Assigned(FNotifyBeginLoad) then begin
    contentLength:= GetHeader32(ARequest, WINHTTP_QUERY_CONTENT_LENGTH);
    FNotifyBeginLoad(Self, contentLength);
  end;
end;

procedure THTTPNotifyLoadResponse.WriteData(const Buffer; ALength: Integer);
begin
  inherited WriteData(Buffer, ALength);
  if Assigned(FNotifyLoading) then
    FNotifyLoading(Self, Buffer, ALength);
end;

{ THTTPStreamResponse }

constructor THTTPStreamResponse.Create(AOutStream: TStream; AOwnStream: Boolean; AIsTerminated: TSilentTerminator; AOnlySuccess: Boolean);
begin
  inherited Create(AOutStream, AOwnStream);
  FIsTerminated:= AIsTerminated;
  FOnlySuccess:= AOnlySuccess;
end;

function THTTPStreamResponse.IsNeedRead(ARequest: HINTERNET): Boolean;
begin
  Result:= inherited IsNeedRead(ARequest);
  if not FOnlySuccess then
    Result:= True;
end;

procedure THTTPStreamResponse.WriteData(const Buffer; ALength: Integer);
begin
  inherited WriteData(Buffer, ALength);
  if Assigned(FIsTerminated) and FIsTerminated then
    Abort;
end;

{ THTTPPostRequest }

constructor THTTPPostRequest.Create;
begin
  FMethod:= 'POST';
end;

{ THTTPAsyncContext }

procedure THTTPAsyncContext.Callback(AInternet: HINTERNET; Status: DWORD; StatusInformation: Pointer;
  StatusInformationLength: DWORD);
var buf: array [0..4097] of Byte;
    bufSize: Integer;
begin
  case Status of
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE,
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE: begin
      if Request.HaveDataToSend then begin
        bufSize:= Request.GetNextDataChunk(buf, SizeOf(buf));
        if bufSize > 0 then begin
          if not WinHttpAPI.WriteData(Handle, @buf, bufSize, nil) then
            RaiseError;
        end;
      end else
        if not WinHttpAPI.ReceiveResponse(Handle, nil) then
          RaiseError;
    end;
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE:
      if Response.IsNeedRead(Handle) then begin
        FReadedSize:= 0;
        //contentLength:= Response.GetHeader32(Handle, WINHTTP_QUERY_CONTENT_LENGTH, 0);
        if not WinHttpAPI.ReadData(Handle, @FBufferData, SizeOf(FBufferData), @FLastReadedSize) then
          RaiseError();
      end else begin
        Response.EndResponse(0);
        FIsClosed:= True;
        DoNotifyEndLoad;
      end;
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE:begin
      Response.WriteData(StatusInformation^, StatusInformationLength);
      Inc(FReadedSize, StatusInformationLength);
      if StatusInformationLength <> SizeOf(FBufferData) then begin
        Response.EndResponse(FReadedSize);
        FIsClosed:= True;
        DoNotifyEndLoad;
      end else
        if not WinHttpAPI.ReadData(Handle, @FBufferData, SizeOf(FBufferData), @FLastReadedSize) then
          RaiseError();
    end;
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING:
      if FDisposed then
        Destroy;
  end;
end;

constructor THTTPAsyncContext.Create(AClient: THTTPClient;
  ARequest: THTTPRequest; AResponse: THTTPResponse; AHandle, AConnection: HINTERNET);
begin
  FClient:= AClient;
  FRequest:= ARequest;
  FResponse:= AResponse;
  AResponse.FContext:= Self;
  FHandle:= AHandle;
  FConnection:= AConnection;
end;

destructor THTTPAsyncContext.Destroy;
begin
  if Handle <> 0 then
    WinHttpAPI.CloseHandle(Handle);

  if Connection <> 0 then
    WinHttpAPI.CloseHandle(Connection);

  if FNeedDestroyAll then begin
    FRequest.Free;
    FResponse.Free;
  end;

  if FNeedDestroyClient then
    FClient.Free;
  inherited;
end;

procedure THTTPAsyncContext.Dispose;
var H: HINTERNET;
begin
  H:= FHandle;
  FHandle:= 0;
  FDisposed:= True;
  WinHttpAPI.CloseHandle(H);
end;

procedure THTTPAsyncContext.DoNotifyEndLoad;
begin
  try
    if Assigned(FOnEndLoad) then
      FOnEndLoad(Self);
  finally
    if DisposeAfterLoad then
      Dispose;
  end;
end;

function THTTPAsyncContext.GetNextDataChunk(var Data;
  ALength: Integer): Integer;
begin
  Result:= FRequest.GetNextDataChunk(Data, ALength);
  Inc(FProcessedDataLength, Result);
end;

procedure THTTPAsyncContext.RaiseError;
var Err: DWORD;
begin
  Err:= GetLastError;
  case Err of
  0, 6:;
  else
    RaiseLastOSError(Err);
  end;
end;

procedure THTTPAsyncContext.ReadNextChunk;
begin
  Callback(Handle, WINHTTP_CALLBACK_STATUS_READ_COMPLETE, @FBufferData[0], FLastReadedSize);
end;

procedure THTTPAsyncContext.Run(AOnEndLoad: THTTPAsyncContextEndLoading; ADisposeAfterLoad: Boolean);
begin
  DisposeAfterLoad:= ADisposeAfterLoad;
  OnEndLoad:= AOnEndLoad;
  Run;
end;

procedure THTTPAsyncContext.Run;
var headers: string;
    h: Pointer;
begin
  if AtomicExchange(FStarted, 1) = 0 then begin
    if Client.IsAsynchronous then
      IsMultiThread:= True;
    headers:= Request.GetHeaders;
    h:= Pointer(headers);
    {need:= ARequest.TotalDataLength;
    if Result = nil then
      bufSize:= ARequest.GetNextDataChunk(buf, SizeOf(buf))
    else
      bufSize:= Result.GetNextDataChunk(buf, SizeOf(buf));

    if not WinHttpAPI.SendRequest(Request, h, Length(headers), @buf, bufSize, need, Result) then
      RaiseLastOSError;}
    if not WinHttpAPI.SendRequest(Handle, h, Length(headers), nil, 0, Request.TotalDataLength, Self) then
      RaiseLastOSError;
  end else
    raise EHTTPAlreadyRunning.Create('Request already started');
end;

procedure THTTPAsyncContext.SetOnEndLoad(const Value: THTTPAsyncContextEndLoading);
begin
  FOnEndLoad := Value;
  if IsClosed then
    DoNotifyEndLoad;
end;

{ THTTPCustomStreamResponse }

constructor THTTPCustomStreamResponse.Create(AOutStream: TStream; AOwnStream: Boolean);
begin
  FOutStream:= AOutStream;
  FStreamOwner:= AOwnStream;
end;

destructor THTTPCustomStreamResponse.Destroy;
begin
  if FStreamOwner then
    FOutStream.Free;
  inherited;
end;

procedure THTTPCustomStreamResponse.WriteData(const Buffer; ALength: Integer);
begin
  inherited WriteData(Buffer, ALength);
  FOutStream.WriteBuffer(Buffer, ALength);
end;

{ THTTPPostRequestStream }

constructor THTTPPostRequestStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream:= AStream;
  FHasData:= True;
end;

function THTTPPostRequestStream.GetNextDataChunk(var Data;
  ALength: Integer): Integer;
begin
  Result:= FStream.Read(Data, ALength);
  FHasData:= ALength = Result;
end;

function THTTPPostRequestStream.HaveDataToSend: Boolean;
begin
  Result:= FHasData;
end;

{ THTTPPostRequestConstBuffer }

constructor THTTPPostRequestConstBuffer.Create(const ABuffer: TArray<Byte>);
begin
  inherited Create;
  FBuffer:= ABuffer;
  FOffset:= 0;
  AddHeader('Content-Length: ' + IntToStr(Length(ABuffer)));
end;

constructor THTTPPostRequestConstBuffer.Create(const ABuffer; ALength: Integer);
var b: TArray<Byte>;
begin
  SetLength(b, ALength);
  Move(ABuffer, b[0], ALength);
  Create(b);
end;

function THTTPPostRequestConstBuffer.GetNextDataChunk(var Data;
  ALength: Integer): Integer;
begin
  Result:= Length(FBuffer) - FOffset;
  if Result > ALength then
    Result:= ALength;
  Move(FBuffer[FOffset], Data, Result);
  Inc(FOffset, Result);
end;

function THTTPPostRequestConstBuffer.HaveDataToSend: Boolean;
begin
  Result:= FOffset < High(FBuffer);
end;

{ EHTTPWrapperException }

constructor EHTTPWrapperException.Create(const AMessage: string; AContext: THTTPAsyncContext);
begin
  inherited Create(AMessage);
  FContext:= AContext;
end;

destructor EHTTPWrapperException.Destroy;
begin
  if Context.DisposeAfterLoad then
    Context.Dispose;

  inherited;
end;

initialization

  WinHttpAPIInitialize;

finalization

  if WinHttpAPI.LibraryHandle <> 0 then begin
    FreeLibrary(WinHttpAPI.LibraryHandle);
    WinHttpAPI.LibraryHandle:= 0;
  end;

end.

