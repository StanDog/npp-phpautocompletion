//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit ParsingIndicatorForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, shellapi, Vcl.ComCtrls;

type
  TParsingIndicatorForm = class(TNppForm)
    ProgressBar1: TProgressBar;
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ParsingIndicatorForm: TParsingIndicatorForm;

implementation

uses cccplugin, Vcl.Themes, ccchelperfunctions;

{$R *.dfm}

procedure TParsingIndicatorForm.FormPaint(Sender: TObject);
begin
  inherited;
  DrawGlassText(Canvas, 12, 'Please wait', 24, 3, 315, 25, DT_CENTER, 'Segoe UI', 12, []);
  DrawGlassText(Canvas, 10, 'Scanning root directory. This is done only'+ sLineBreak +'after directory was changed and only once!', 24, 57, 315, 34, DT_CENTER, 'Segoe UI', 8, []);
  DrawGlassText(Canvas, 10, ' If you see this popup although root directory not changed,'+ sLineBreak +'please consider to use a sub-folder (with less files). ', 24, 94, 315, 30, DT_CENTER, 'Segoe UI', 8, []);
end;

end.
