unit FastXML;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections,
  Winapi.Windows;

type
  TXMLPartialElement = class;
  TXMLElementArray = array of TXMLPartialElement;
  PXMLElementParserOptions = ^TXMLElementParserOptions;

  TXMLNewElement = function (const ElementName: string; ParseOptions: PXMLElementParserOptions; Root: Pointer): Pointer of object;
  TXMLNewAttribute = procedure(const AttributeName, AttributeValue: string; Element: Pointer) of object;
  TXMLCloseElement = procedure (Element: Pointer) of object;
  TXMLNewText = procedure (const Value: string; Element: Pointer) of object;
  TXMLNewProcessingInstruction = procedure (const Name, Params: string) of object;

  TXMLElementParserOptions = record
    OnNewAttribute: TXMLNewAttribute;
    OnText: TXMLNewText;
    SkipSpaceText: Boolean;
    OnCData: TXMLNewText;
    OnComment: TXMLNewText;
    OnNewProcessingInstruction: TXMLNewProcessingInstruction;
    OnNewInnerElement: TXMLNewElement; //Open child element
    OnCloseChildElement: TXMLCloseElement; //Close child element
    OnCloseElement: TXMLCloseElement; //Close current element
  end;

  TXMLReader = class
  private
    FRoot: TXMLPartialElement;
    FOwned: Boolean;
    FStream: TStream;
    FBufferOffset: Int64;
    FEncodingOwner: Boolean;
    FEncoding: TEncoding;
    FSkipedPart: Integer;
    FAfterReadedPosition: Int64;
    FWrongXML: Boolean;
    FXMLVersion: string;
    FIsStandalone: Boolean;
  protected
    function GetRoot: TXMLPartialElement;
    function Init: string;
    function ReadEncoding(var Str: string; var Position: Integer; var enc: string): Boolean;
    function ReadName(var Str: string; var Position: Integer; out LastIndex: Integer; var Name: string): Boolean;
    function SkipName(var Str: string; var Position: Integer; out LastIndex: Integer): Boolean;
    function ReadAttributeValue(var Str: string; var Position: Integer; out LastIndex: Integer; var Value: string): Boolean;
    function SkipAttributeValue(var Str: string; var Position: Integer; out LastIndex: Integer): Boolean;
    function ReadComment(var Str: string; var Position: Integer; out LastIndex: Integer; var Value: string): Boolean;
    function SkipComment(var Str: string; var Position: Integer; out LastIndex: Integer): Boolean;
    procedure ExtendStringFromBuffer(var str: string); //inline;
    function ReadNextBufString: string; //inline;
    function ExtendString(var Str: string; var Offset: Integer; out LastIndex: Integer): Boolean; //inline;
    function ExtendStringWithSafe(var Str: string; var Offset, LastIndex, BeginPos: Integer): Boolean; //inline;
    procedure SetBufferOffset(const Value: Int64); //inline;
    function Skip(var Str: string; var Position: Integer; out LastIndex: Integer; MinLen: Integer): Boolean; overload;
    function Skip(var Str: string; var Position: Integer; MinLen: Integer): Boolean; overload; inline;
    function SkipByMark(var Str: string; var Position: Integer; const Mark: string): Boolean;
    function SkipByMarkWithSave(var Str, Value: string; var Position: Integer; const Mark: string): Boolean;
    procedure Parse(const BeginValue: string; const DefaultParserOptions: TXMLElementParserOptions);
  public
    property XMLVersion: string read FXMLVersion;
    property IsStandalone: Boolean read FIsStandalone;
    property Root: TXMLPartialElement read GetRoot;
    function XPath(const Path: string; CreateInnerBlocks: Boolean = False): TXMLElementArray;
    function XPathFirst(const Path: string): TXMLPartialElement;
    procedure LoadFromStream(AStream: TStream; const DefaultParserOptions: TXMLElementParserOptions; AOwned: Boolean = False);
    procedure LoadFromFile(const FileName: string; const DefaultParserOptions: TXMLElementParserOptions);
    procedure ParseString(const Value: string; const DefaultParserOptions: TXMLElementParserOptions);
    destructor Destroy; override;
  end;

  TXMLElementMonoParserOptions = record
    ParseAttrib: Boolean;
    ParseText: Boolean;
    SkipSpaceText: Boolean;
    ParseCData: Boolean;
    ParseComments: Boolean;
    ParseInnerElements: Boolean;
    ParseProcessingInstructions: Boolean;
  end;
  PXMLElementMonoParserOptions = ^TXMLElementMonoParserOptions;

  TXMLMonoNewElement = function (const ElementName: string; var ParseOptions: TXMLElementMonoParserOptions; Root: Pointer): Pointer of object;

  TXMLMonoReader = class (TXMLReader)
  private
    FOnNewElement: TXMLMonoNewElement;
    FOnNewAttribute: TXMLNewAttribute;
    FOnCloseElement: TXMLCloseElement;
    FOnNewComment: TXMLNewText;
    FOnNewText: TXMLNewText;
    FOnCData: TXMLNewText;
    FOnNewProcessingInstruction: TXMLNewProcessingInstruction;
  protected
    function OnNewInnerElement(const ElementName: string; ParseOptions: PXMLElementParserOptions; Root: Pointer): Pointer;
    function GenerateOptions(const ParserOptions: TXMLElementMonoParserOptions): TXMLElementParserOptions;
  public
    DefaultParserOptions: TXMLElementMonoParserOptions;
    constructor Create;
    procedure LoadFromStream(AStream: TStream; AOwned: Boolean = False);
    procedure LoadFromFile(const FileName: string);
    procedure ParseString(const Value: string);
    property OnNewElement: TXMLMonoNewElement read FOnNewElement write FOnNewElement;
    property OnNewAttribute: TXMLNewAttribute read FOnNewAttribute write FOnNewAttribute;
    property OnCloseElement: TXMLCloseElement read FOnCloseElement write FOnCloseElement;
    property OnNewComment: TXMLNewText read FOnNewComment write FOnNewComment;
    property OnNewText: TXMLNewText read FOnNewText write FOnNewText;
    property OnCData: TXMLNewText read FOnCData write FOnCData;
    property OnNewProcessingInstructions: TXMLNewProcessingInstruction read FOnNewProcessingInstruction write FOnNewProcessingInstruction;
  end;

  TXMLNodeType = (ntText, ntProcessingInstruction, ntNode, ntCData, ntComment);

  TXMLNode = class
  private
  protected
  public
    Value: string;
    NodeType: TXMLNodeType;
    constructor Create(AValue: string; ANodeType: TXMLNodeType);
  end;

  TXMLElement = class (TXMLNode)
  private
    FAttributes: TDictionary<string, string>;
    FNodes: TList<TXMLNode>;
  protected
  public
    property Attributes: TDictionary<string, string> read FAttributes;
    property Nodes: TList<TXMLNode> read FNodes;
    constructor Create(AName: string; ANodeType: TXMLNodeType = ntNode);
    destructor Destroy; override;
  end;

  TXMLDocument = class
  private
    FFullDocument: TList<TXMLNode>;
    FElementsById: TDictionary<string, TList<TXMLElement>>;
  protected
    FRoot: TXMLElement;
    function OnNewElement(const ElementName: string; var ParseOptions: TXMLElementMonoParserOptions; Root: Pointer): Pointer; virtual;
    procedure OnNewAttribute(const AttributeName, AttributeValue: string; Element: Pointer); virtual;
    procedure OnCloseElement(Element: Pointer); virtual;
    procedure OnNewComment(const Value: string; Element: Pointer); virtual;
    procedure OnNewText(const Value: string; Element: Pointer); virtual;
    procedure OnCData(const Value: string; Element: Pointer); virtual;
    procedure OnNewProcessingInstructions(const Name, Params: string); virtual;
    procedure OnDeleteIds(Sender: TObject; const Item: TList<TXMLElement>; Action: TCollectionNotification);
  public
    property Root: TXMLElement read FRoot;
    property FullDocument: TList<TXMLNode> read FFullDocument;
    property ElementsById: TDictionary<string, TList<TXMLElement>> read FElementsById;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function XPathFirst(const Path: string): TXMLElement;
    procedure InitReader(Reader: TXMLMonoReader); virtual;
  end;

  TXMLPartialElement= class
  private
    FOwner: TXMLReader;
    FStartPosition: Int64;
    FAttributes: TDictionary<string, string>;
    FFullAttributes: Boolean;
    FName: string;
  protected
    procedure LoadAttributes;
    function GetAttribute(const Index: string): string;
    function GetAttributesCount: Integer;
  public
    constructor Create(AOwner: TXMLReader; AStartPosition: Int64; AName: string);
    function NextSibling: TXMLPartialElement;
    function NextElementSibling: TXMLPartialElement;
    function FirstChild: TXMLPartialElement;
    function FirstElementChild: TXMLPartialElement;
    property Attribute[const Index: string]: string read GetAttribute;
    function HasAttribute(const Name: string): Boolean;
    property AttributesCount: Integer read GetAttributesCount;
    property Name: string read FName;
  end;

type
  TStaticCharArray = array [0..0] of Char;
  PStaticCharArray = ^TStaticCharArray;

implementation

const
  XMLDecl = '<?xml';

{ TXMLReader }

destructor TXMLReader.Destroy;
begin
  if FEncodingOwner then
    FEncoding.Free;
  if FOwned then
    FStream.Free;
  inherited;
end;

function TXMLReader.ExtendString(var Str: string; var Offset: Integer;
  out LastIndex: Integer): Boolean;
var tmp: string;
begin
  tmp:= ReadNextBufString;
  if tmp = '' then
    Exit(False);
  Str:= Concat(PChar(@Str[Offset]), tmp);
  Offset:= Low(string);
  LastIndex:= High(Str);
  Result:= True;
end;

procedure TXMLReader.ExtendStringFromBuffer(var str: string);
var Buffer: array [0..4095] of Byte;
    Len, BufLen, OldLen: Integer;
    OldPosition: Int64;
begin
  OldPosition:= FStream.Position;
  BufLen:= FStream.Read(Buffer[0], SizeOf(Buffer));
  Len:= FEncoding.GetCharCount(Buffer, 0, BufLen);
  OldLen:= Length(str) + Low(string);
  SetLength(str, Length(str) + Len);
  FEncoding.GetChars(Buffer, 0, BufLen, Slice(PStaticCharArray(@str[OldLen])^, Len), 0);
  SetBufferOffset(OldPosition + FEncoding.GetByteCount(str, OldLen, Len));
end;

function TXMLReader.ExtendStringWithSafe(var Str: string; var Offset,
  LastIndex, BeginPos: Integer): Boolean;
var tmp: string;
begin
  tmp:= ReadNextBufString;
  if tmp = '' then
    Exit(False);
  Str:= Concat(PChar(@Str[BeginPos]), tmp);
  Dec(Offset, BeginPos - Low(string));
  BeginPos:= Low(string);
  LastIndex:= High(Str);
  Result:= True;
end;

function TXMLReader.GetRoot: TXMLPartialElement;
begin

end;

function TXMLReader.Init: string;
var i, NextPos: Integer;
    s: string;
    version, Len, h: Integer;
    open: Char;
    tmp: string;
    Buffer: array [0..4095] of Byte;
    CodePage: Cardinal;
begin
  if FEncodingOwner then
    FEncoding.Free;
  FEncoding:= nil;
  FEncodingOwner:= False;
  SetBufferOffset(0);
  Len:= FStream.Read(Buffer[0], SizeOf(Buffer));
  FBufferOffset:= FStream.Position;
  i:= 0;
  if (Len >= 3) and (Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF) then begin
    Inc(i, 3);
    FEncoding:= TEncoding.UTF8;
  end else if (Len >= 2) and (Buffer[0] = $FF) and (Buffer[1] = $FE) then begin
    Inc(i, 2);
    FEncoding:= TEncoding.Unicode;
  end else if (Len >= 2) and (Buffer[0] = $FE) and (Buffer[1] = $FF) then begin
    Inc(i, 2);
    FEncoding:= TEncoding.BigEndianUnicode;
  end else if (Len >= 2) and (Buffer[1] = 0) then
    FEncoding:= TEncoding.Unicode
  else if (Len >= 2) and (Buffer[0] = 0) then
    FEncoding:= TEncoding.BigEndianUnicode
  else
    FEncoding:= TEncoding.UTF8;

  NextPos:= FEncoding.GetCharCount(Buffer, i, Len - i);
  SetLength(s, NextPos);
  FEncoding.GetChars(Buffer, i, Len - i, Slice(PStaticCharArray(@s[Low(string)])^, NextPos), 0);
  SetBufferOffset(i + FEncoding.GetByteCount(s));
  FIsStandalone:= False;
  h:= High(s);
  if StrLIComp(PChar(s), XMLDecl, Length(XMLDecl)) = 0 then begin
    FWrongXML:= True;
    i:= Length(XMLDecl) + Low(string);
    if not Skip(s, i, h, 7) then
      Exit;
    if StrLComp(PChar(@s[i]), 'version', 7) = 0 then begin
      Inc(i, 7);
      if not Skip(s, i, h, 1) then
        Exit;
      if s[i] <> '=' then
        Exit;
      Inc(i);
      if not Skip(s, i, h, 5) then
        Exit;
      open:= s[i];
      if (open <> '"') and (open <> '''') then
        Exit;
      Inc(i);
      version:= i;
      if (s[i] <> '1') and (s[i + 1] <> '.') then
        Exit;
      Inc(i, 2);
      while (s[i] >= '0') and (s[i] <= '9') do begin
        while (s[i] >= '0') and (s[i] <= '9') do Inc(i);
        if i > h then begin
          if not ExtendStringWithSafe(s, i, h, version) then
            Exit;
        end;
      end;
      if open <> s[i] then
        Exit;
      SetString(FXMLVersion, PChar(@s[version]), i - version);
      Inc(i);
    end;
    if not ReadEncoding(s, i, tmp) then
      Exit;
    if tmp <> '' then try
      FEncoding:= TEncoding.GetEncoding(tmp);
      FEncodingOwner:= True;
    except
      Exit;
    end;
    if not Skip(s, i, h, 10) then
      Exit;
    if StrLComp(PChar(@s[i]), 'standalone', 10) = 0 then begin
      Inc(i, 10);
      if not Skip(s, i, h, 1) then
        Exit;
      if s[i] <> '=' then
        Exit;
      Inc(i);
      if not Skip(s, i, h, 4) then
        Exit;
      open:= s[i];
      if StrLComp(PChar(@s[i + 1]), 'yes', 3) = 0 then begin
        Inc(i, 4);
        FIsStandalone:= True;
      end else if StrLComp(PChar(@s[i + 1]), 'no', 2) = 0 then begin
        Inc(i, 3);
        FIsStandalone:= False;
      end else
        Exit;
      if open <> s[i] then
        Exit;
      Inc(i);
    end;
    if not Skip(s, i, h, 2) then
      Exit;
    if StrLComp(PChar(@s[i]), '?>', 2) <> 0 then
      Exit;
    Inc(i, 2);
    Result:= s;
    FSkipedPart:= i;
    FAfterReadedPosition:= FBufferOffset;
    FWrongXML:= False;
  end else begin
    FXMLVersion:= '1.0';
    FIsStandalone:= False;
    Result:= ''
  end;
end;

procedure TXMLReader.LoadFromFile(const FileName: string; const DefaultParserOptions: TXMLElementParserOptions);
var Stream: TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  LoadFromStream(Stream, DefaultParserOptions, True);
end;

procedure TXMLReader.LoadFromStream(AStream: TStream; const DefaultParserOptions: TXMLElementParserOptions; AOwned: Boolean);
begin
  FOwned:= AOwned;
  FStream:= AStream;
  FBufferOffset:= -1;
  Parse(Init, DefaultParserOptions);
end;

procedure TXMLReader.Parse(const BeginValue: string; const DefaultParserOptions: TXMLElementParserOptions);
var Count: Integer;
    CurOpt, Restrict: TXMLElementParserOptions;
    s, Name: string;
    i, h: Integer;
    CurElement: Pointer;
    Names: TList<string>;
    Elements: TList;
    Options: TList<TXMLElementParserOptions>;
  procedure CloseCurrentTag;
  begin
    if Assigned(CurOpt.OnCloseElement) then
      CurOpt.OnCloseElement(CurElement);
    CurOpt:= Options.Last;
    if Assigned(CurOpt.OnCloseChildElement) then
      CurOpt.OnCloseChildElement(CurElement);
    if Assigned(CurOpt.OnNewInnerElement) then begin
      Elements.Delete(Elements.Count - 1);
      CurElement:= Elements.Last;
    end;
    Options.Delete(Options.Count - 1);
    Names.Delete(Names.Count - 1);
  end;

  function _ReadComment: Boolean;
  var vStart: Integer;
      tmp, Value: string;
  begin
    if Assigned(CurOpt.OnComment) then begin
      Result:= ReadComment(s, i, h, Value);
      CurOpt.OnComment(Value, CurElement);
    end else begin
      Result:= SkipComment(s, i, h);
    end;
  end;
var Value: string;
    NewElement: TXMLNewElement;
label _dropElement;
begin
  s:= BeginValue;

  if DefaultParserOptions.SkipSpaceText then begin
    if not Skip(s, FSkipedPart, h, 2) then
      Exit;
  end else if FSkipedPart + 2 >= High(s) then
    ExtendString(s, FSkipedPart, h);

  if FSkipedPart > High(s) then
    Exit;

  {Restrict.ParseAttrib:= Assigned(FOnNewAttribute);
  Restrict.ParseText:= Assigned(FOnNewText);
  Restrict.ParseCData:= Assigned(FOnCData);
  Restrict.ParseComments:= Assigned(FOnNewComment);
  Restrict.ParseInnerElements:= Assigned(FOnNewElement);
  Restrict.ParseProcessingInstructions:= Assigned(FOnNewProcessingInstructions);}

  if not Assigned(DefaultParserOptions.OnNewInnerElement) then
    Exit;

  CurElement:= nil;
  Elements:= nil;
  Options:= nil;
  Names:= TList<string>.Create;
  try
    Options:= TList<TXMLElementParserOptions>.Create;
    Elements:= TList.Create;
    Elements.Add(nil);
    CurOpt:= DefaultParserOptions;
    {CurOpt.ParseAttrib:= DefaultParserOptions.ParseAttrib and Restrict.ParseAttrib;
    CurOpt.ParseText:= DefaultParserOptions.ParseText and Restrict.ParseText;
    CurOpt.ParseCData:= DefaultParserOptions.ParseCData and Restrict.ParseCData;
    CurOpt.ParseComments:= DefaultParserOptions.ParseComments and Restrict.ParseComments;
    CurOpt.ParseInnerElements:= DefaultParserOptions.ParseInnerElements and Restrict.ParseInnerElements;
    CurOpt.ParseProcessingInstructions:= DefaultParserOptions.ParseAttrib and Restrict.ParseProcessingInstructions;
    CurOpt.SkipSpaceText:= DefaultParserOptions.SkipSpaceText;}

    i:= FSkipedPart;

    while i <= h do begin
      while s[i] = '<' do begin
        Inc(i);

        case s[i] of
          '!': begin
            if h - i < 2 then
              if not ExtendString(s, i, h) then
                goto _dropElement;
            if (s[i + 1] = '-') and (s[i + 2] = '-') then begin
              Inc(i, 3);
              if not _ReadComment then
                goto _dropElement;
            end else begin
              if h - i < 7 then
                if not ExtendString(s, i, h) then
                  goto _dropElement;
              if StrLComp(@PChar(Pointer(s))[i], '[CDATA[', 7) = 0 then begin
                if Elements.Count = 0 then
                  raise Exception.Create('Error Message');
                Inc(i, 8);
                if Assigned(CurOpt.OnCData) then
                  {$MESSAGE WARN 'Not realised'}
                  raise ENotImplemented.Create('CDATA')
                else
                  SkipByMark(s, i, ']]>');
              end else if StrLComp(@PChar(Pointer(s))[i], 'DOCTYPE', 7) = 0 then begin
                if Elements.Count <> 0 then
                  raise Exception.Create('Error Message');
              end else
                goto _dropElement;
            end;
          end;
          '?': begin
            Inc(i);
            if Assigned(CurOpt.OnNewProcessingInstruction) then begin
              if ReadName(s, i, h, Name) then begin
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
                if not SkipByMarkWithSave(s, Value, i, '?>') then
                  goto _dropElement;
                Inc(i, 2);
                CurOpt.OnNewProcessingInstruction(Name, Value);
              end;
            end else begin
              if not SkipByMark(s, i, '?>') then
                goto _dropElement;
            end;
          end;
          //Close tag
          '/': begin
            Inc(i);
            if ReadName(s, i, h, Name) then begin
              if Name <> Names.Last then
                raise Exception.Create('Error Message');
              CloseCurrentTag;
              if not Skip(s, i, h, 1) then
                goto _dropElement;
              if s[i] <> '>' then
                goto _dropElement;
              Inc(i);
            end else
              goto _dropElement;
          end;
        else
          if ReadName(s, i, h, Name) then begin
            Names.Add(Name);
            Options.Add(CurOpt);
            NewElement:= CurOpt.OnNewInnerElement;
            FillChar(CurOpt, SizeOf(CurOpt), 0);
            if Assigned(NewElement) then begin
              CurOpt.SkipSpaceText:= DefaultParserOptions.SkipSpaceText;
              CurElement:= NewElement(Name, @CurOpt, CurElement);
              Elements.Add(CurElement);
              {CurOpt.ParseAttrib:= CurOpt.ParseAttrib and Restrict.ParseAttrib;
              CurOpt.ParseText:= CurOpt.ParseText and Restrict.ParseText;
              CurOpt.ParseCData:= CurOpt.ParseCData and Restrict.ParseCData;
              CurOpt.ParseComments:= CurOpt.ParseComments and Restrict.ParseComments;
              CurOpt.ParseInnerElements:= CurOpt.ParseInnerElements and Restrict.ParseInnerElements;
              CurOpt.ParseProcessingInstructions:= CurOpt.ParseAttrib and Restrict.ParseProcessingInstructions; }
            end else begin
              CurElement:= nil;
              CurOpt.SkipSpaceText:= True;
            end;
            if not Skip(s, i, h, 1) then
              Break;
            if Assigned(CurOpt.OnNewAttribute) then begin
              while (s[i] <> '/') and (s[i] <> '>') do begin
                if not ReadName(s, i, h, Name) then
                  goto _dropElement;
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
                if s[i] <> '=' then
                  goto _dropElement;
                Inc(i);
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
                if not ReadAttributeValue(s, i, h, Value) then
                  goto _dropElement;

                CurOpt.OnNewAttribute(Name, Value, CurElement);

                if not Skip(s, i, h, 1) then
                  goto _dropElement;
              end;
            end else begin
              while (s[i] <> '/') and (s[i] <> '>') do begin
                if not SkipName(s, i, h) then
                  goto _dropElement;
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
                if s[i] <> '=' then
                  goto _dropElement;
                Inc(i);
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
                if not SkipAttributeValue(s, i, h) then
                  goto _dropElement;
                if not Skip(s, i, h, 1) then
                  goto _dropElement;
              end;
            end;
            if s[i] = '/' then begin
              //Empty Tag
              Inc(i);
              if i > h then
                if not ExtendString(s, i, h) then
                  goto _dropElement;
              if s[i] = '>' then begin
                Inc(i);
                CloseCurrentTag;
              end else
                goto _dropElement;
            end else if s[i] = '>' then begin
              {$MESSAGE WARN 'Don''t needed???'}
              {if not CurOpt.ParseInnerElements then
                CurOpt.ParseAttrib:= False; }
              Inc(i);
            end else
              goto _dropElement;
          end else
            goto _dropElement;
        end;
        if CurOpt.SkipSpaceText then
          Skip(s, i, h, 2)
        else begin
          h:= High(s);
          if i + 2 >= h then
            ExtendString(s, i, h);
        end;
      end;
      if Assigned(CurOpt.OnText) then begin
        if not Assigned(CurOpt.OnNewProcessingInstruction) then begin
          SkipByMarkWithSave(s, Name, i, '<');
        end else
          {$MESSAGE WARN 'Not realised'}
          raise ENotImplemented.Create('ParseText');
        CurOpt.OnText(Name, CurElement);
        if CurOpt.SkipSpaceText then
          Skip(s, i, h, 2)
        else begin
          h:= High(s);
          if (i + 2 >= h) and not ExtendString(s, i, h) then
            goto _dropElement;
        end;
      end else begin
        Inc(i);
        Skip(s, i, h, 2);
      end;
    end;
    _dropElement:
  finally
    Names.Free;
    Options.Free;
    Elements.Free;
  end;
end;

procedure TXMLReader.ParseString(const Value: string; const DefaultParserOptions: TXMLElementParserOptions);
begin
  FOwned:= True;
  FStream:= TMemoryStream.Create;
  FBufferOffset:= -1;
  FSkipedPart:= Low(string);
  Parse(Value, DefaultParserOptions);
end;

function TXMLReader.ReadAttributeValue(var Str: string; var Position: Integer; out LastIndex: Integer;
  var Value: string): Boolean;
var open: Char;
    s, ref: string;
    i, h, vStart: Integer;
begin
  s:= Str;
  i:= Position;
  open:= s[i];
  Inc(i);
  vStart:= i;
  h:= High(s);
  if (open <> '''') and (open <> '"') then
    Exit(False);
  Result:= False;
  while ((i <= h) or ExtendStringWithSafe(s, i, h, vStart)) and
      (s[i] <> '<') and (s[i] <> open) do begin
    Inc(i);
    {if s[i] = '&' then begin
      Inc(i);
      if h - i < 2 then
        if not ExtendStringWithSafe(s, i, h, vStart) then
          Break;
      if s[i] = '#' then begin

      end else begin
        if not ReadName(s, i, ref) then
          Break;
      end;
    end;}
  end;
  if s[i] = open then begin
    Value:= Copy(s, vStart, i - vStart);
    Inc(i);
    Result:= True;
  end;
  Str:= s;
  Position:= i;
  LastIndex:= h;
end;

function TXMLReader.ReadComment(var Str: string; var Position: Integer;
  out LastIndex: Integer; var Value: string): Boolean;
var s: string;
    i, vStart: Integer;
    h: Integer;
begin
  s:= Str;
  i:= Position;
  h:= High(s);
  vStart:= i;

  while (i <= h - 3 + Low(string)) or ExtendStringWithSafe(s, i, h, vStart) do begin
    while (s[i] <> '-') and (i <= h) do
      Inc(i);
    if i <= h - 3 + Low(string) then begin
      if StrLComp(PChar(@s[i]), '-->', 3) = 0 then begin
        Value:= Copy(s, vStart, i - vStart);
        Inc(i, 3);
        Position:= i;
        Str:= s;
        Exit(True);
      end else begin
        while ((i <= h - 3 + Low(string)) or ExtendStringWithSafe(s, i, h, vStart)) and (s[i] = '-') do
          Inc(i);
      end;
    end;
  end;
  if vStart = Low(string) then
    Value:= s
  else
    Value:= Copy(s, vStart);
  Position:= i;
  Str:= s;
  Result:= False;
end;

function TXMLReader.ReadEncoding(var Str: string; var Position: Integer;
  var enc: string): Boolean;
var s, tmp: string;
    i, h: Integer;
    encStart: Integer;
    open: Char;
begin
  s:= Str;
  i:= Position;
  h:= High(s);
  if not Skip(s, i, h, 8) then
    Exit(False);

  if StrLComp(PChar(@s[i]), 'encoding', 8) = 0 then begin
    Inc(i, 8);
    if not Skip(s, i, h, 1) then
      Exit(False);
    if s[i] <> '=' then
      Exit(True);
    Inc(i);
    if not Skip(s, i, h, 1) then
      Exit(False);
    open:= s[i];
    Inc(i);
    if ((open <> '"') and (open <> '''')) or not (((s[i] >= 'A') and (s[i] <= 'Z')) or ((s[i] >= 'a') and (s[i] <= 'z'))) then
      Exit(True);
    encStart:= i;
    while ((s[i] >= '0') and (s[i] <= '9')) or ((s[i] >= 'A') and (s[i] <= 'Z')) or
          ((s[i] >= 'a') and (s[i] <= 'z')) or (s[i] = '.') or (s[i] = '_') or (s[i] = '-') do begin
      while ((s[i] >= '0') and (s[i] <= '9')) or ((s[i] >= 'A') and (s[i] <= 'Z')) or
          ((s[i] >= 'a') and (s[i] <= 'z')) or (s[i] = '.') or (s[i] = '_') or (s[i] = '-') do
        Inc(i);
      if i > h then begin
        if not ExtendStringWithSafe(s, i, h, encStart) then
          Exit;
      end;
    end;
    if s[i] <> open then
      Exit(True);
    enc:= Copy(s, encStart, i - encStart);
    Inc(i);
  end;

  Str:= S;
  Position:= i;
  Result:= True;
end;

function GetCharIndex(): Integer; inline;
begin

end;

function TXMLReader.ReadName(var Str: string; var Position: Integer; out LastIndex: Integer;
  var Name: string): Boolean;
var s, tmp: string;
    p, index, h, nameStart: Integer;
begin
  s:= Str;
  p:= Position;
  h:= High(s);   
  nameStart:= p;
  if (p > h) and not ExtendStringWithSafe(s, p, h, nameStart) then begin
    Name:= '';
    Exit(False);
  end;
  case Word(s[p]) of
    Ord(':'), Ord('A')..Ord('Z'), Ord('_'), Ord('a')..Ord('z'), $C0..$D6, $D8..$F6, $F8..$2FF, $370..$37D,
    $37F..$1FFF, $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$D7FF, $F900..$FDCF, $FDF0..$FFFD:
      Inc(p);
    //$10000..$EFFFF
    $D800..$DB7F: begin
      if p >= h then
        raise Exception.Create('Error Message');
      Inc(p, 2);
    end
  else
    Name:= '';
    Result:= False;
    Exit;
  end;

  while (p <= h) or ExtendStringWithSafe(s, p, h, nameStart) do
    case Word(s[p]) of
      Ord(':'), Ord('A')..Ord('Z'), Ord('_'), Ord('a')..Ord('z'), $C0..$D6, $D8..$F6, $F8..$2FF, $370..$37D,
      $37F..$1FFF, $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$D7FF, $F900..$FDCF, $FDF0..$FFFD,
      Ord('-'), Ord('.'), Ord('0')..Ord('9'), $B7, $0300..$036F, $203F..$2040:
        Inc(p);
      //#$10000..#$EFFFF
      $D800..$DB7F: begin
        if p >= h then
          raise Exception.Create('Error Message');
        Inc(p, 2);
      end
    else
      Break;
    end;

  if (nameStart = Low(string)) and (p - 1 = h)  then
    Name:= s
  else
    Name:= Copy(s, nameStart, p - nameStart);
  Str:= s;
  Position:= p;
  LastIndex:= h;
  Result:= p <= h;
end;

function TXMLReader.ReadNextBufString: string;
var Buffer: array [0..4095] of Byte;
    Len, BufLen, NewBufLen: Integer;
    OldPosition: Int64;
begin
  OldPosition:= FStream.Position;
  BufLen:= FStream.Read(Buffer[0], SizeOf(Buffer));
  if BufLen = 0 then
    Exit('');
  Len:= FEncoding.GetCharCount(Buffer, 0, BufLen);
  while Len = 0 do begin
    Dec(BufLen);
    if BufLen <= 0 then
      raise Exception.Create('Wrong string encoding');
    Len:= FEncoding.GetCharCount(Buffer, 0, BufLen);
  end;
  Result:= '';
  SetLength(Result, Len);
  FEncoding.GetChars(Buffer, 0, BufLen, Slice(PStaticCharArray(@Result[Low(string)])^, Len), 0);
  NewBufLen:= FEncoding.GetByteCount(Result);
  if BufLen <> NewBufLen then
    Len:= BufLen;
  SetBufferOffset(OldPosition + NewBufLen);
end;

procedure TXMLReader.SetBufferOffset(const Value: Int64);
begin
  if FBufferOffset <> Value then begin
    FBufferOffset:= Value;
    FStream.Position:= Value;
  end;
end;

function TXMLReader.Skip(var Str: string; var Position: Integer; out LastIndex: Integer;
  MinLen: Integer): Boolean;
var j, h: Integer;
    tmp, s: string;
begin
  j:= Position;
  s:= Str;
  h:= High(s);
  if (h - j + 1 >= MinLen) or ExtendString(s, j, h) then
    repeat
      while (s[j] = ' ') or (s[j] = #9) or (s[j] = #10) or (s[j] = #13) do
        Inc(j);
      if (h - j + 1 < MinLen) and not ExtendString(s, j, h) then
        Break;
    until (s[j] <> ' ') and (s[j] <> #9) and (s[j] <> #10) and (s[j] <> #13);
  Position:= j;
  Str:= s;
  Result:= (h - j + 1 >= MinLen);
  LastIndex:= High(Str);
end;

function TXMLReader.Skip(var Str: string; var Position: Integer;
  MinLen: Integer): Boolean;
var h: Integer;
begin
  Result:= Skip(Str, Position, h, MinLen);
end;

function TXMLReader.SkipAttributeValue(var Str: string;
  var Position: Integer; out LastIndex: Integer): Boolean;
var open: Char;
    s, ref: string;
    i, h: Integer;
begin
  s:= Str;
  i:= Position;
  open:= s[i];
  Inc(i);
  h:= High(s);
  if (open <> '''') and (open <> '"') then
    Exit(False);
  Result:= False;
  while ((i <= h) or ExtendString(s, i, h)) and
      (s[i] <> '<') and (s[i] <> open) do begin
    Inc(i);
  end;
  if s[i] = open then begin
    Inc(i);
    Result:= True;
  end;
  Str:= s;
  Position:= i;
  LastIndex:= h;
end;

function TXMLReader.SkipByMark(var Str: string; var Position: Integer;
  const Mark: string): Boolean;
var j, h, ml: Integer;
    tmp, s: string;
begin
  j:= Position;
  s:= Str;
  h:= High(s);
  ml:= High(Mark);
  while (j <= h - ml + Low(string)) or ExtendString(s, j, h) do begin
    while (s[j] <> Mark[Low(string)]) and (j <= h) do
      Inc(j);
    if (j <= h - ml + Low(string)) then begin
      if (StrLComp(PChar(@PChar(Pointer(s))[j - 1]), @PChar(Pointer(Mark))[0], ml + 1 - Low(string)) = 0) then begin
        Inc(j, ml + 1 - Low(string));
        Position:= j;
        Str:= s;
        Exit(True);
      end else
        Inc(j);
    end;
  end;
  Position:= j;
  Str:= s;
  Result:= False;
end;

function TXMLReader.SkipByMarkWithSave(var Str, Value: string; var Position: Integer;
  const Mark: string): Boolean;
var j, h, ml, vStart: Integer;
    tmp, s: string;
begin
  j:= Position;
  vStart:= j;
  s:= Str;
  h:= High(s);
  ml:= High(Mark);
  while (j <= h - ml + Low(string)) or ExtendStringWithSafe(s, j, h, vStart) do begin
    while (s[j] <> Mark[Low(string)]) and (j <= h) do
      Inc(j);
    if (j <= h - ml + Low(string)) and (StrLComp(PWideChar(@s[j]), PWideChar(@Mark[Low(string)]), ml + 1 - Low(string)) = 0) then begin
      Value:= Copy(s, vStart, j - vStart);
      //Inc(j, ml + 1 - Low(string));
      Position:= j;
      Str:= s;
      Exit(True);
    end;
  end;
  Value:= Copy(s, vStart);
  Position:= j;
  Str:= s;
  Result:= False;
end;

function TXMLReader.SkipComment(var Str: string; var Position: Integer;
  out LastIndex: Integer): Boolean;
var s: string;
    i: Integer;
    h: Integer;
begin
  s:= Str;
  i:= Position;
  h:= High(s);

  Result:= False;
  while (i <= h - 3 + Low(string)) or ExtendString(s, i, h) do begin
    while (s[i] <> '-') and (i <= h) do
      Inc(i);
    if (i <= h - 3 + Low(string)) then begin
      if StrLComp(PChar(@s[i]), '-->', 3) = 0 then begin
        Inc(i, 3);
        Result:= True;
        Break;
      end else begin
        while ((i <= h - 3 + Low(string)) or ExtendString(s, i, h)) and (s[i] = '-') do
          Inc(i);
      end;
    end;
  end;
  Position:= i;
  Str:= s;
end;

function TXMLReader.SkipName(var Str: string; var Position: Integer; out LastIndex: Integer): Boolean;
var s, tmp: string;
    p, index, h: Integer;
begin
  s:= Str;
  p:= Position;
  h:= High(s);
  case Word(s[p]) of
    Ord(':'), Ord('A')..Ord('Z'), Ord('_'), Ord('a')..Ord('z'), $C0..$D6, $D8..$F6, $F8..$2FF, $370..$37D,
    $37F..$1FFF, $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$D7FF, $F900..$FDCF, $FDF0..$FFFD:
      Inc(p);
    //$10000..$EFFFF
    $D800..$DB7F: begin
      if p >= h then
        raise Exception.Create('Error Message');
      Inc(p, 2);
    end
  else
    Result:= False;
    Exit;
  end;

  while (p <= h) or ExtendString(s, p, h) do begin
    case Word(s[p]) of
      Ord(':'), Ord('A')..Ord('Z'), Ord('_'), Ord('a')..Ord('z'), $C0..$D6, $D8..$F6, $F8..$2FF, $370..$37D,
      $37F..$1FFF, $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$D7FF, $F900..$FDCF, $FDF0..$FFFD,
      Ord('-'), Ord('.'), Ord('0')..Ord('9'), $B7, $0300..$036F, $203F..$2040:
        Inc(p);
      //#$10000..#$EFFFF
      $D800..$DB7F: begin
        if p >= h then
          raise Exception.Create('Error Message');
        Inc(p, 2);
      end
    else
      Break;
    end;
  end;
  Str:= s;
  Position:= p;
  LastIndex:= h;
  Result:= p <= h;
end;

function TXMLReader.XPath(const Path: string;
  CreateInnerBlocks: Boolean): TXMLElementArray;
begin

end;

function TXMLReader.XPathFirst(const Path: string): TXMLPartialElement;
var CurLen, ofs: Integer;
    Levels: TArray<string>;
    CurLevelBegin, CurLevelEnd: PWideChar;
    P: PWideChar;
    s: string;
begin
  P:= PWideChar(Path);
  CurLevelEnd:= StrScan(P, '/');
  CurLevelBegin:= P;
  CurLen:= Integer(CurLevelEnd - CurLevelBegin);
  s:= '';
  ofs:= FSkipedPart;
  SetBufferOffset(FAfterReadedPosition);
  Result:= nil;
  while Skip(s, ofs, 1 + CurLen) and (s[ofs] = '<') do begin
    Inc(ofs);
    if (StrLIComp(CurLevelBegin, @s[ofs], CurLen) = 0) and
        (s[ofs + CurLen] in [#9, #10, #13, '>', ' ', '/']) then begin

    end;

  end;
end;

{ TXMLPartialElement }

constructor TXMLPartialElement.Create(AOwner: TXMLReader; AStartPosition: Int64;
  AName: string);
begin

end;

function TXMLPartialElement.FirstChild: TXMLPartialElement;
begin

end;

function TXMLPartialElement.FirstElementChild: TXMLPartialElement;
begin

end;

function TXMLPartialElement.GetAttribute(const Index: string): string;
begin

end;

function TXMLPartialElement.GetAttributesCount: Integer;
begin

end;

function TXMLPartialElement.HasAttribute(const Name: string): Boolean;
begin

end;

procedure TXMLPartialElement.LoadAttributes;
begin

end;

function TXMLPartialElement.NextElementSibling: TXMLPartialElement;
begin

end;

function TXMLPartialElement.NextSibling: TXMLPartialElement;
begin

end;

{ TXMLElement }

constructor TXMLElement.Create(AName: string; ANodeType: TXMLNodeType);
begin
  if (ANodeType <> TXMLNodeType.ntNode) and (ANodeType <> TXMLNodeType.ntNode) then
    raise Exception.CreateFmt('Wrong NodeType: %d', [Integer(ANodeType)]);
  inherited Create(AName, ANodeType);
  FAttributes:= TDictionary<string, string>.Create;
  FNodes:= TList<TXMLNode>.Create;
end;

destructor TXMLElement.Destroy;
var
  i: Integer;
begin
  FAttributes.Free;
  if FNodes <> nil then begin
    for i := 0 to FNodes.Count - 1 do
      FNodes[i].Free;
    FNodes.Destroy;
  end;
  inherited;
end;

{ TXMLNode }

constructor TXMLNode.Create(AValue: string; ANodeType: TXMLNodeType);
begin
  Value:= AValue;
  NodeType:= ANodeType;
end;

{ TXMLDocument }

procedure TXMLDocument.Clear;
var
  i: Integer;
begin
  if FFullDocument <> nil then begin
    for i := 0 to FFullDocument.Count - 1 do
      FFullDocument[i].Free;
    FFullDocument.Clear;
  end;
  FElementsById.Clear;
  FRoot:= nil;
end;

constructor TXMLDocument.Create;
begin
  FFullDocument:= TList<TXMLNode>.Create;
  FElementsById:= TDictionary<string, TList<TXMLElement>>.Create;
  FElementsById.OnValueNotify:= OnDeleteIds;
end;

destructor TXMLDocument.Destroy;
var
  i: Integer;
begin
  Clear;
  FFullDocument.Free;
  FElementsById.Free;
  inherited;
end;

procedure TXMLDocument.InitReader(Reader: TXMLMonoReader);
begin
  Reader.OnNewElement:= OnNewElement;
  Reader.OnNewAttribute:= OnNewAttribute;
  Reader.OnNewComment:= OnNewComment;
  Reader.OnNewText:= OnNewText;
  Reader.OnNewProcessingInstructions:= OnNewProcessingInstructions;
  Reader.OnCloseElement:= OnCloseElement;
  Reader.OnCData:= OnCData;
end;

procedure TXMLDocument.OnCData(const Value: string; Element: Pointer);
begin
  if Element = nil then
    FullDocument.Add(TXMLNode.Create(Value, TXMLNodeType.ntCData))
  else
    TXMLElement(Element).Nodes.Add(TXMLNode.Create(Value, TXMLNodeType.ntCData));
end;

procedure TXMLDocument.OnCloseElement(Element: Pointer);
var id: string;
    l: TList<TXMLElement>;
begin
  if Element <> nil then begin
    if TXMLElement(Element).Attributes.TryGetValue('id', id) then begin
      if not ElementsById.TryGetValue(id, l) then begin
        l:= TList<TXMLElement>.Create;
        ElementsById.Add(id, l);
      end;
      l.Add(TXMLElement(Element));
    end;
  end;
end;

procedure TXMLDocument.OnDeleteIds(Sender: TObject; const Item: TList<TXMLElement>;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

procedure TXMLDocument.OnNewAttribute(const AttributeName,
  AttributeValue: string; Element: Pointer);
begin
  if Element <> nil then begin
    TXMLElement(Element).Attributes.Add(AttributeName, AttributeValue);
  end;
end;

procedure TXMLDocument.OnNewComment(const Value: string; Element: Pointer);
begin
  if Element = nil then
    FullDocument.Add(TXMLNode.Create(Value, TXMLNodeType.ntComment))
  else
    TXMLElement(Element).Nodes.Add(TXMLNode.Create(Value, TXMLNodeType.ntComment));
end;

function TXMLDocument.OnNewElement(const ElementName: string;
  var ParseOptions: TXMLElementMonoParserOptions; Root: Pointer): Pointer;
begin
  Result:= TXMLElement.Create(ElementName);
  if Root = nil then begin
    FullDocument.Add(TXMLElement(Result));
    FRoot:= TXMLElement(Result);
  end else begin
    TXMLElement(Root).Nodes.Add(TXMLElement(Result));
  end;
end;

procedure TXMLDocument.OnNewProcessingInstructions(const Name, Params: string);
begin

end;

procedure TXMLDocument.OnNewText(const Value: string; Element: Pointer);
begin
  if Element = nil then
    FullDocument.Add(TXMLNode.Create(Value, TXMLNodeType.ntText))
  else
    TXMLElement(Element).Nodes.Add(TXMLNode.Create(Value, TXMLNodeType.ntText));
end;

function TXMLDocument.XPathFirst(const Path: string): TXMLElement;
var i, old: Integer;
    e: TXMLElement;
  j: Integer;
  s: string;
begin
  old:= 1;
  i:= Pos('/', Path);
  if i > 0 then
    s:= Copy(Path, old, i - old)
  else
    s:= Copy(Path, old);
  if Root.Value = s then begin
    Result:= Root;
    if i > 0 then begin
      old:= i + 1;
      i:= Pos('/', Path, old);
      while i > 0 do begin
        s:= Copy(Path, old, i - old);
        e:= Result;
        for j := 0 to e.Nodes.Count - 1 do
          if (e.Nodes[j] is TXMLElement) and (e.Nodes[j].Value = s) then begin
            Result:= TXMLElement(e.Nodes[j]);
            Break;
          end;
        if e = Result then begin
          Result:= nil;
          Exit;
        end;
        old:= i + 1;
        i:= Pos('/', Path, old);
      end;
      s:= Copy(Path, old);
      e:= Result;
      for j := 0 to e.Nodes.Count - 1 do
        if (e.Nodes[j] is TXMLElement) and (e.Nodes[j].Value = s) then begin
          Result:= TXMLElement(e.Nodes[j]);
          Break;
        end;
      if e = Result then
        Result:= nil;
    end;
  end else
    Result:= nil;
end;

{ TXMLMonoReader }

constructor TXMLMonoReader.Create;
begin
  DefaultParserOptions.SkipSpaceText:= True;
end;

function TXMLMonoReader.GenerateOptions(const ParserOptions: TXMLElementMonoParserOptions): TXMLElementParserOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  if ParserOptions.ParseAttrib then
    Result.OnNewAttribute:= FOnNewAttribute;
  if ParserOptions.ParseText then
    Result.OnText:= FOnNewText;
  if ParserOptions.ParseCData then
    Result.OnCData:= FOnCData;
  if ParserOptions.ParseComments then
    Result.OnComment:= FOnNewComment;
  if ParserOptions.ParseInnerElements then begin
    Result.OnCloseElement:= FOnCloseElement;
    Result.OnNewInnerElement:= OnNewInnerElement;
  end;
  if ParserOptions.ParseProcessingInstructions then
    Result.OnNewProcessingInstruction:= FOnNewProcessingInstruction;

  Result.SkipSpaceText:= ParserOptions.SkipSpaceText;
end;

procedure TXMLMonoReader.LoadFromFile(const FileName: string);
begin
  inherited LoadFromFile(FileName, GenerateOptions(DefaultParserOptions));
end;

procedure TXMLMonoReader.LoadFromStream(AStream: TStream; AOwned: Boolean);
begin
  inherited LoadFromStream(AStream, GenerateOptions(DefaultParserOptions), AOwned);
end;

function TXMLMonoReader.OnNewInnerElement(const ElementName: string;
  ParseOptions: PXMLElementParserOptions; Root: Pointer): Pointer;
var Opt: TXMLElementMonoParserOptions;
begin
  if not Assigned(FOnNewElement) then
    raise Exception.Create('Error Message');

  Opt:= DefaultParserOptions;
  Result:= FOnNewElement(ElementName, Opt, Root);
  ParseOptions^:= GenerateOptions(Opt);
end;

procedure TXMLMonoReader.ParseString(const Value: string);
begin
  inherited ParseString(Value, GenerateOptions(DefaultParserOptions));
end;

end.
