//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit AboutForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, shellapi;

type
  TAboutForm = class(TNppForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Label4Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses cccplugin;

{$R *.dfm}

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
    if Key = VK_ESCAPE then
    begin
      ModalResult := mrOk;
      //Close();
    end;
end;

procedure TAboutForm.Label4Click(Sender: TObject);
var
  emailLabel: TLabel;
begin
  inherited;
  emailLabel := Sender as TLabel;
  ShellExecute(Handle, 'open', PWideChar('mailto:' + emailLabel.Caption + '?subject=ACCPC%20plugin%20v' + sAppVersionDisplay), nil, nil, SW_SHOW);
end;

end.
