unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FileManager, Vcl.ComCtrls,
  System.Generics.Collections;

type
  TfMain = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    bAddMod: TButton;
    tv: TTreeView;
    bRefresh: TButton;
    ListView1: TListView;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure bAddModClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
    FM: TFileManager;
    O: TObjectList<TObject>;
  public
    { Public declarations }
    procedure ShowFiles;
  end;

var
  fMain: TfMain;

implementation

uses Heroes3LOD;

{$R *.dfm}

procedure TfMain.Button1Click(Sender: TObject);
var Directory: string;
    Search: TSearchRec;
begin
  Directory:= Edit1.Text;
  if FindFirst(Directory + '*', faDirectory, Search) = 0 then
    repeat
      if ((Search.Name <> '.') and (Search.Name <> '..')) then begin
        Search.Name:= AnsiLowerCase(Search.Name);
          //Memo1.Lines.Add(Search.Name);
      end;
    until FindNext(Search) <> 0;
end;

procedure TfMain.bAddModClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    FM.ConnectAddOnAs(TLODAddOn.Create(TFileStream.Create(OpenDialog1.FileName, fmOpenRead), True), '');
    ShowFiles;
  end;
end;

procedure TfMain.bRefreshClick(Sender: TObject);
begin
  ShowFiles;
end;

procedure TfMain.FormCreate(Sender: TObject);
var r: string;
begin
  GetDir(0, r);
  FM:= TFileManager.Create(r + PathDelim + 'temp');
  O:= TObjectList<TObject>.Create;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FM.Free;
  O.Free;
end;

procedure TfMain.ListView1DblClick(Sender: TObject);
var stream: TStream;
    fs: TFileStream;
begin
  if ListView1.Selected <> nil then begin
    SaveDialog1.FileName:= ListView1.Selected.Caption;
    if SaveDialog1.Execute then begin
      stream:= FM.GetFileStream(ListView1.Selected.Caption, []);
      fs:= TFileStream.Create(SaveDialog1.FileName, fmCreate);
      fs.CopyFrom(stream, stream.Size);
      fs.Free;
      stream.Free;
    end;
  end;
end;

procedure TfMain.ShowFiles;
var v: TStringList;
  i: Integer;
begin
  v:= TStringList.Create;
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;
    FM.GetFilesInDir('', Edit1.Text, v);
    for i := 0 to v.Count - 1 do
      ListView1.Items.Add.Caption:= v[i];
  finally
    ListView1.Items.EndUpdate;
    v.Free;
  end;
end;

end.
