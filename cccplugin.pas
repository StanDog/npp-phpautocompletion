//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit cccplugin;

interface

uses
  NppPlugin, SysUtils, Windows, SciSupport, AboutForms, ClassesListDockingForms, AnalyzerPHP,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, shlwapi, accpc_popup_helper_class,
  ccchelperfunctions, System.WideStrUtils, System.Classes, System.Generics.Collections,
  ParsingIndicatorForms, Vcl.ComCtrls;

type
  TCCCPlugin = class(TNppPlugin)
  public
    accpc_popup_helper_class: Taccpc_popup_helper_class;
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure FuncSettings;
    procedure FuncClassList;
    procedure FuncAbout;
    procedure DoNppnToolbarModification; override;
    procedure DoNppnReady; override;
    procedure DoNppnShutdown; override;
    procedure BeNotified(sn: PSCNotification); override;
  end;

procedure _FuncSettings; cdecl;
procedure _FuncClassList; cdecl;
procedure _FuncAbout; cdecl;

var
  Npp: TCCCPlugin;
  foregroundTimer: TTimer;
  settingsPath: String;
  settingsFile: String;
  settingsCompatibleVersions: System.Generics.Collections.TList<integer>;
  analyzerFile: String;

  php_analyzer: TAnalyzerPHP;

  accpc_popup: TForm;
  accpc_popup_listbox: TListBox;
  accpc_popup_panel: TPanel;

  accpc_settings: TForm;
  accpc_settings_leRootDirectory: TLabeledEdit;
  accpc_settings_btnSave: TButton;
  accpc_settings_btnChooseRootDir: TButton;
  accpc_settings_btnCancel: TButton;
  accpc_settings_cbEnableAutocompletion: TCheckBox;
  accpc_settings_cbShowPrivate: TCheckBox;
  accpc_settings_cbShowParamTypeCasting: TCheckBox;
  accpc_settings_cbResetEverytime: TCheckBox;
  accpc_settings_lblAutocompletionTimeout: TLabel;
  accpc_settings_tbAutocompletionTimeout: TTrackBar;
  accpc_settings_lblAutocompletionTimeoutValue: TLabel;
  accpc_settings_var_oldleRootDirectory: String;

  bInitialized: Boolean;

  iAppVersion: UINT32;
  sAppVersion: String;
  sAppVersionDisplay: String;

implementation

{ TCCCPlugin }

procedure labelClickTest2(Sender: TObject);
var
  btn: TButton;
begin
  btn := Sender as TButton;
  Application.MessageBox(PWideChar(btn.Caption), '');
end;

constructor TCCCPlugin.Create;
var
  {sk: TShortcutKey;}
  style: LONG;
begin
  inherited;

  iAppVersion := 1004001;
  sAppVersion := '1004001';
  sAppVersionDisplay := '1.4.1';

  // Define compatible settings file versions
  settingsCompatibleVersions := System.Generics.Collections.TList<integer>.Create();
  settingsCompatibleVersions.Add(iAppVersion);
  settingsCompatibleVersions.Add(1004000);
  settingsCompatibleVersions.Add(1003000);
  settingsCompatibleVersions.Add(1002003);
  settingsCompatibleVersions.Add(1002002);
  settingsCompatibleVersions.Add(1002001);
  settingsCompatibleVersions.Add(1002000);
  settingsCompatibleVersions.Add(1001000);
  settingsCompatibleVersions.Add(1000001);

  bInitialized := false;

  //****************************************************************
  //**                Create function helper class                **
  //****************************************************************
  self.accpc_popup_helper_class := Taccpc_popup_helper_class.Create;

  //****************************************************************
  //**                   Create analyzer class                    **
  //****************************************************************
  php_analyzer := TAnalyzerPHP.Create();

  //****************************************************************
  //**                    Setup some variables                    **
  //****************************************************************
  self.PluginName := 'PHP Autocompletion';

  //****************************************************************
  //**                 Create menu entries in NPP                 **
  //****************************************************************
  {sk.IsCtrl := true; sk.IsAlt := true; sk.IsShift := false;
  sk.Key := #118; // CTRL ALT SHIFT F7
  self.AddFuncItem('Replace ACCPC', _FuncCCC, sk);}

  self.AddFuncItem('Settings', _FuncSettings);
  self.AddFuncItem('PHP Class Inspector', _FuncClassList);
  self.AddFuncItem('---', nil);
  self.AddFuncItem('About ACCPC...', _FuncAbout);

  //****************************************************************
  //**                    Create settings form                    **
  //****************************************************************
  accpc_settings := TForm.Create(nil);
  accpc_settings.Name := 'accpc_settings';
  accpc_settings.BorderStyle := bsDialog;
  accpc_settings.BorderIcons := [biSystemMenu];
  accpc_settings.Scaled := false;
  accpc_settings.Caption := 'Settings';
  accpc_settings.Position := poScreenCenter;
  accpc_settings.KeyPreview := true;
  accpc_settings.ClientWidth := 320;
  accpc_settings.ClientHeight := 224;
  accpc_settings.OnKeyDown := self.accpc_popup_helper_class.settingsFormKeyDown;
  accpc_settings.OnShow := self.accpc_popup_helper_class.settingsFormShow;

  // Create checkbox
  accpc_settings_cbEnableAutocompletion := TCheckBox.Create(accpc_settings);
  accpc_settings_cbEnableAutocompletion.Parent := accpc_settings;
  accpc_settings_cbEnableAutocompletion.Left := 16;
  accpc_settings_cbEnableAutocompletion.Top := 16;
  accpc_settings_cbEnableAutocompletion.Width := 290;
  accpc_settings_cbEnableAutocompletion.Caption := 'Enable PHP autocompletion';
  accpc_settings_cbEnableAutocompletion.Checked := true;

  // Create checkbox
  accpc_settings_cbShowPrivate := TCheckBox.Create(accpc_settings);
  accpc_settings_cbShowPrivate.Parent := accpc_settings;
  accpc_settings_cbShowPrivate.Left := 16;
  accpc_settings_cbShowPrivate.Top := 40;
  accpc_settings_cbShowPrivate.Width := 290;
  accpc_settings_cbShowPrivate.Caption := 'Show private && protected attributes && methods';
  accpc_settings_cbShowPrivate.Checked := false;

  // Create checkbox
  accpc_settings_cbShowParamTypeCasting := TCheckBox.Create(accpc_settings);
  accpc_settings_cbShowParamTypeCasting.Parent := accpc_settings;
  accpc_settings_cbShowParamTypeCasting.Left := 16;
  accpc_settings_cbShowParamTypeCasting.Top := 64;
  accpc_settings_cbShowParamTypeCasting.Width := 290;
  accpc_settings_cbShowParamTypeCasting.Caption := 'Show parameter type casting';
  accpc_settings_cbShowParamTypeCasting.Checked := false;

  // Create checkbox
  accpc_settings_cbResetEverytime := TCheckBox.Create(accpc_settings);
  accpc_settings_cbResetEverytime.Parent := accpc_settings;
  accpc_settings_cbResetEverytime.Left := 16;
  accpc_settings_cbResetEverytime.Top := 88;
  accpc_settings_cbResetEverytime.Width := 290;
  accpc_settings_cbResetEverytime.Caption := 'Parse new / changed files only';
  accpc_settings_cbResetEverytime.Checked := true;

  // Create label: Autocomplation timeout
  accpc_settings_lblAutocompletionTimeout := TLabel.Create(accpc_settings);
  accpc_settings_lblAutocompletionTimeout.Parent := accpc_settings;
  accpc_settings_lblAutocompletionTimeout.Left := 16;
  accpc_settings_lblAutocompletionTimeout.Top := 112;
  accpc_settings_lblAutocompletionTimeout.Caption := 'Autocompletion typing timeout';

  // Create trackbar: Autocompletion timeout
  accpc_settings_tbAutocompletionTimeout := TTrackBar.Create(accpc_settings);
  accpc_settings_tbAutocompletionTimeout.Parent := accpc_settings;
  accpc_settings_tbAutocompletionTimeout.Left := accpc_settings_lblAutocompletionTimeout.Left + accpc_settings_lblAutocompletionTimeout.Width + 10;
  accpc_settings_tbAutocompletionTimeout.Top := 108;
  accpc_settings_tbAutocompletionTimeout.Width := accpc_settings.ClientWidth - accpc_settings_tbAutocompletionTimeout.Left - 50;
  accpc_settings_tbAutocompletionTimeout.Height := 25;
  accpc_settings_tbAutocompletionTimeout.TickMarks := TTickMark.tmBoth;
  accpc_settings_tbAutocompletionTimeout.TickStyle := TTickStyle.tsNone;
  accpc_settings_tbAutocompletionTimeout.Min := 5;
  accpc_settings_tbAutocompletionTimeout.Max := 30;
  accpc_settings_tbAutocompletionTimeout.Position := accpc_settings_tbAutocompletionTimeout.Min;
  accpc_settings_tbAutocompletionTimeout.OnChange := self.accpc_popup_helper_class.autocompletionTimeoutChange;

  // Create label: Autocomplation timeout
  accpc_settings_lblAutocompletionTimeoutValue := TLabel.Create(accpc_settings);
  accpc_settings_lblAutocompletionTimeoutValue.Parent := accpc_settings;
  accpc_settings_lblAutocompletionTimeoutValue.Left := accpc_settings_tbAutocompletionTimeout.Left + accpc_settings_tbAutocompletionTimeout.Width + 5;
  accpc_settings_lblAutocompletionTimeoutValue.Top := 112;
  accpc_settings_lblAutocompletionTimeoutValue.Caption := '0.5 sec';

  // Create labeled edit
  accpc_settings_leRootDirectory := TLabeledEdit.Create(accpc_settings);
  accpc_settings_leRootDirectory.Parent := accpc_settings;
  accpc_settings_leRootDirectory.Left := 16;
  accpc_settings_leRootDirectory.Top := 160;
  accpc_settings_leRootDirectory.Width := 260;  // If accpc_settings_btnChooseRootDir.Visible=True: 260, otherwise: 290
  accpc_settings_leRootDirectory.EditLabel.Caption := 'Root directory (where to look for PHP files)';

  // Create "Choose root directory" dialog
  accpc_settings_btnChooseRootDir := TButton.Create(accpc_settings);
  accpc_settings_btnChooseRootDir.Parent := accpc_settings;
  accpc_settings_btnChooseRootDir.Left := accpc_settings_leRootDirectory.Left + accpc_settings_leRootDirectory.Width + 10;
  accpc_settings_btnChooseRootDir.Top := 160-1;  // Seems to be due to style for Windows controls
  accpc_settings_btnChooseRootDir.Height := 23-1;
  accpc_settings_btnChooseRootDir.Width := 25;
  accpc_settings_btnChooseRootDir.Caption := '...';
  accpc_settings_btnChooseRootDir.OnClick := self.accpc_popup_helper_class.chooseRootDirClick;
  accpc_settings_btnChooseRootDir.Visible := True;  // Currently I'm still testing how to solve the problem with overlapping windows

  // Create save button
  accpc_settings_btnSave := TButton.Create(accpc_settings);
  accpc_settings_btnSave.Parent := accpc_settings;
  accpc_settings_btnSave.Left := 79;
  accpc_settings_btnSave.Top := 191;
  accpc_settings_btnSave.Height := 23;
  accpc_settings_btnSave.Caption := 'Save';
  accpc_settings_btnSave.ModalResult := mrOk;

  // Create cancel button
  accpc_settings_btnCancel := TButton.Create(accpc_settings);
  accpc_settings_btnCancel.Parent := accpc_settings;
  accpc_settings_btnCancel.Left := 167;
  accpc_settings_btnCancel.Top := 191;
  accpc_settings_btnCancel.Height := 23;
  accpc_settings_btnCancel.Caption := 'Cancel';
  accpc_settings_btnCancel.ModalResult := mrCancel;

  //****************************************************************
  //**                     Create popup form                      **
  //****************************************************************
  accpc_popup := TForm.Create(nil);
  accpc_popup.Name := 'accpc_popup';
  accpc_popup.BorderStyle := bsNone;
  accpc_popup.Scaled := false;
  accpc_popup.Caption := 'accpc_popup';
  accpc_popup.ClientWidth := 390;
  accpc_popup.ClientHeight := 200;
  accpc_popup.Constraints.MinWidth := 100;
  accpc_popup.Constraints.MinHeight := 40;
  accpc_popup.OnResize := self.accpc_popup_helper_class.popupFormResize;
  accpc_popup.OnShow := self.accpc_popup_helper_class.popupFormShow;

  // Create listbox on popup
  accpc_popup_listbox := TListBox.Create(accpc_popup);
  accpc_popup_listbox.Parent := accpc_popup;
  accpc_popup_listbox.Align := alClient;
  accpc_popup_listbox.Style := lbOwnerDrawVariable;
  accpc_popup_listbox.AutoComplete := true;  // We still can use default autocomplete functionality by placing the name of attribute / method (it will not be visible - see below)
  accpc_popup_listbox.DoubleBuffered := true;
  accpc_popup_listbox.Font.Name := 'Tahoma';
  accpc_popup_listbox.Font.Size := 8;
  accpc_popup_listbox.OnKeyDown := self.accpc_popup_helper_class.popupListboxKeyDown;
  accpc_popup_listbox.OnDblClick := self.accpc_popup_helper_class.popupListboxDblClick;
  accpc_popup_listbox.OnDrawItem := self.accpc_popup_helper_class.popupListboxDrawItem;

  // Create panel on popup
  accpc_popup_panel := TPanel.Create(accpc_popup);
  accpc_popup_panel.Parent := accpc_popup;
  accpc_popup_panel.Height := 22;
  accpc_popup_panel.Align := alBottom;
  accpc_popup_panel.Visible := false;  // hide - reserved for future use

  // Remove popup's window title bar (leave just dragable borders)
  style := GetWindowLong(accpc_popup.Handle, GWL_STYLE);
  SetWindowLong(accpc_popup.Handle, GWL_STYLE, style or WS_SIZEBOX);

  // Prevent showing taskbar button when forms showed
  style := GetWindowLong(accpc_settings.Handle, GWL_EXSTYLE);
  SetWindowLong(accpc_settings.Handle, GWL_EXSTYLE, style or WS_EX_TOOLWINDOW);
  style := GetWindowLong(accpc_popup.Handle, GWL_EXSTYLE);
  SetWindowLong(accpc_popup.Handle, GWL_EXSTYLE, style or WS_EX_TOOLWINDOW);

  //****************************************************************
  //**         Create timer to set windows to foreground          **
  //**             (this is a bug which puts N++ over             **
  //**              our windows which stays on top)               **
  //****************************************************************
  foregroundTimer := TTimer.Create(nil);
  foregroundTimer.Interval := 50;
  foregroundTimer.OnTimer := self.accpc_popup_helper_class.foregroundTimerOnTimer;

end;

destructor TCCCPlugin.Destroy;
begin
  // Delete timers first
  if Assigned(foregroundTimer) then
  begin
    foregroundTimer.Enabled := false;
    FreeAndNil(foregroundTimer);
  end;

  if Assigned(settingsCompatibleVersions) then
  begin
    settingsCompatibleVersions.Free();
    settingsCompatibleVersions := nil;
  end;

  // Delete PHP Analyzer
  if Assigned(php_analyzer) then
  begin
    FreeAndNil(php_analyzer);
  end;

  // Delete popup form
  if Assigned(accpc_popup) then
  begin
    FreeAndNil(accpc_popup_listbox);
    FreeAndNil(accpc_popup_panel);
    FreeAndNil(accpc_popup);  // Must be deleted after the other components has been deleted
  end;

  // Delete settings form
  if Assigned(accpc_settings) then
  begin
    FreeAndNil(accpc_settings_leRootDirectory);
    FreeAndNil(accpc_settings_btnSave);
    FreeAndNil(accpc_settings_btnChooseRootDir);
    FreeAndNil(accpc_settings_btnCancel);
    FreeAndNil(accpc_settings_cbEnableAutocompletion);
    FreeAndNil(accpc_settings_cbShowPrivate);
    FreeAndNil(accpc_settings_cbShowParamTypeCasting);
    FreeAndNil(accpc_settings_cbResetEverytime);
    FreeAndNil(accpc_settings_lblAutocompletionTimeout);
    FreeAndNil(accpc_settings_tbAutocompletionTimeout);
    FreeAndNil(accpc_settings_lblAutocompletionTimeoutValue);
    FreeAndNil(accpc_settings);  // Must be deleted after the other components has been deleted
  end;

  // Delete parsing indicator form
  if Assigned(ParsingIndicatorForm) then
  begin
    ParsingIndicatorForm.Free();
  end;

  // Delete helper function class
  if Assigned(accpc_popup_helper_class) then
  begin
    FreeAndNil(accpc_popup_helper_class);
  end;

  inherited;
end;

procedure TCCCPlugin.DoNppnShutdown;
var
  analyzer: TFileStream;
  value_uInt8: UINT8;
  value_uInt16: UINT16;
  value_uInt32: UINT32;
  value_Int32: INT32;
  entry1: INT32;
  entry2: INT32;
begin
  // Save settings
  WritePrivateProfileString('settings', 'version', PWideChar(sAppVersion), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'EnableAutocompletion', PWideChar(String(iif(accpc_settings_cbEnableAutocompletion.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'ShowPrivate', PWideChar(String(iif(accpc_settings_cbShowPrivate.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'ResetEverytime', PWideChar(String(iif(accpc_settings_cbResetEverytime.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'RootDirectory', PWideChar(accpc_settings_leRootDirectory.Text), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'ClientWidth', PWideChar(IntToStr(accpc_popup.ClientWidth)), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'ClientHeight', PWideChar(IntToStr(accpc_popup.ClientHeight)), PWideChar(settingsPath + settingsFile));
  WritePrivateProfileString('settings', 'AutocompletionTimeout', PWideChar(IntToStr(accpc_settings_tbAutocompletionTimeout.Position)), PWideChar(settingsPath + settingsFile));

  // Prepare PHP analyzer object for saving
  // Step 1: Remove temporarily defined classes (which are defined in current Notepad++ session but not saved anywhere)
  for entry1 := php_analyzer.classLinkingList.Count-1 downto 0 do
  begin
    if
    (
      (php_analyzer.classLinkingList.Items[entry1].fileIndices.Count = 0) or
      (php_analyzer.classLinkingList.Items[entry1].fileIndices.Contains(-1))
    )
    then begin
      // Delete class and linking entry
      php_analyzer.classes.Delete(php_analyzer.classLinkingList.Items[entry1].classIndex);
      php_analyzer.classLinkingList.Delete(entry1);
    end;
  end;

  // Save PHP analyzer object
  analyzer := TFileStream.Create(settingsPath + analyzerFile, fmCreate);
  analyzer.Write(iAppVersion, sizeof(iAppVersion));

  // Write classes
  if (php_analyzer.classes.Count > 0) then
  begin
    // Write collection ID
    value_uInt8 := 1;
    analyzer.Write(value_uInt8, sizeof(value_uInt8));

    // Write collection entries count
    value_uInt32 := php_analyzer.classes.Count;
    analyzer.Write(value_uInt32, sizeof(value_uInt32));

    // Write entries
    for entry1 := 0 to php_analyzer.classes.Count-1 do
    begin
      // Write class name length
      value_uInt16 := Length(php_analyzer.classes.Items[entry1].name);
      analyzer.Write(value_uInt16, sizeof(value_uInt16));

      // Write class name
      if (value_uInt16 > 0) then
        analyzer.Write(php_analyzer.classes.Items[entry1].name[1], value_uInt16 * sizeof(char));

      // Write derived class name length
      value_uInt16 := Length(php_analyzer.classes.Items[entry1].derivedFromClass);
      analyzer.Write(value_uInt16, sizeof(value_uInt16));

      // Write derived class name
      if (value_uInt16 > 0) then
        analyzer.Write(php_analyzer.classes.Items[entry1].derivedFromClass[1], value_uInt16 * sizeof(char));

      // Write offset position in file
      value_uInt32 := php_analyzer.classes.Items[entry1].fileOffset;
      analyzer.Write(value_uInt32, sizeof(value_uInt32));

      // Write attributes count
      value_uInt32 := php_analyzer.classes.Items[entry1].attributes.Count;
      analyzer.Write(value_uInt32, sizeof(value_uInt32));

      // Write attribute entries
      for entry2 := 0 to php_analyzer.classes.Items[entry1].attributes.Count-1 do
      begin
        // Write attribute visibility
        case php_analyzer.classes.Items[entry1].attributes.Items[entry2].visibility of
          asvUnknown: value_uInt8 := 0;
          asvPublic: value_uInt8 := 1;
          asvPrivate: value_uInt8 := 2;
          asvProtected: value_uInt8 := 3;
          else value_uInt8 := 0;
        end;
        analyzer.Write(value_uInt8, sizeof(value_uInt8));

        // Write attribute static
        analyzer.Write(php_analyzer.classes.Items[entry1].attributes.Items[entry2].bStatic, sizeof(php_analyzer.classes.Items[entry1].attributes.Items[entry2].bStatic));

        // Write attribute const
        analyzer.Write(php_analyzer.classes.Items[entry1].attributes.Items[entry2].bConst, sizeof(php_analyzer.classes.Items[entry1].attributes.Items[entry2].bConst));

        // Write attribute name length
        value_uInt16 := Length(php_analyzer.classes.Items[entry1].attributes.Items[entry2].name);
        analyzer.Write(value_uInt16, sizeof(value_uInt16));

        // Write attribute name
        if (value_uInt16 > 0) then
          analyzer.Write(php_analyzer.classes.Items[entry1].attributes.Items[entry2].name[1], value_uInt16 * sizeof(char));

        // Write offset position in file
        value_uInt32 := php_analyzer.classes.Items[entry1].attributes.Items[entry2].fileOffset;
        analyzer.Write(value_uInt32, sizeof(value_uInt32));
      end;

      // Write methods count
      value_uInt32 := php_analyzer.classes.Items[entry1].methods.Count;
      analyzer.Write(value_uInt32, sizeof(value_uInt32));

      // Write method entries
      for entry2 := 0 to php_analyzer.classes.Items[entry1].methods.Count-1 do
      begin
        // Write method visibility
        case php_analyzer.classes.Items[entry1].methods.Items[entry2].visibility of
          asvUnknown: value_uInt8 := 0;
          asvPublic: value_uInt8 := 1;
          asvPrivate: value_uInt8 := 2;
          asvProtected: value_uInt8 := 3;
          else value_uInt8 := 0;
        end;
        analyzer.Write(value_uInt8, sizeof(value_uInt8));

        // Write method static
        analyzer.Write(php_analyzer.classes.Items[entry1].methods.Items[entry2].bStatic, sizeof(php_analyzer.classes.Items[entry1].methods.Items[entry2].bStatic));

        // Write method name length
        value_uInt16 := Length(php_analyzer.classes.Items[entry1].methods.Items[entry2].name);
        analyzer.Write(value_uInt16, sizeof(value_uInt16));

        // Write method name
        if (value_uInt16 > 0) then
          analyzer.Write(php_analyzer.classes.Items[entry1].methods.Items[entry2].name[1], value_uInt16 * sizeof(char));

        // Write method parameters length
        value_uInt32 := Length(php_analyzer.classes.Items[entry1].methods.Items[entry2].parameters.Text);
        analyzer.Write(value_uInt32, sizeof(value_uInt32));

        // Write method parameters
        if (value_uInt32 > 0) then
          analyzer.Write(php_analyzer.classes.Items[entry1].methods.Items[entry2].parameters.Text[1], value_uInt32 * sizeof(char));

        // Write offset position in file
        value_uInt32 := php_analyzer.classes.Items[entry1].methods.Items[entry2].fileOffset;
        analyzer.Write(value_uInt32, sizeof(value_uInt32));
      end;
    end;
  end;

  // Write constants
  if (php_analyzer.constants.Count > 0) then
  begin
    // Write collection ID
    value_uInt8 := 2;
    analyzer.Write(value_uInt8, sizeof(value_uInt8));

    // Write collection entries count
    value_uInt32 := php_analyzer.constants.Count;
    analyzer.Write(value_uInt32, sizeof(value_uInt32));

    // Write entries
    for entry1 := 0 to php_analyzer.constants.Count-1 do
    begin
      // Write constant name length
      value_uInt16 := Length(php_analyzer.constants.Items[entry1].name);
      analyzer.Write(value_uInt16, sizeof(value_uInt16));

      // Write constant name
      if (value_uInt16 > 0) then
        analyzer.Write(php_analyzer.constants.Items[entry1].name[1], value_uInt16 * sizeof(char));

      // Write constant value length
      value_uInt16 := Length(php_analyzer.constants.Items[entry1].value);
      analyzer.Write(value_uInt16, sizeof(value_uInt16));

      // Write constant value
      if (value_uInt16 > 0) then
        analyzer.Write(php_analyzer.constants.Items[entry1].value[1], value_uInt16 * sizeof(char));

      // Write case-sensitivity
      analyzer.Write(php_analyzer.constants.Items[entry1].case_sensitive, sizeof(php_analyzer.constants.Items[entry1].case_sensitive));
    end;
  end;

  // Write checksum database
  if (php_analyzer.dbfiles.Count > 0) then
  begin
    // Write collection ID
    value_uInt8 := 3;
    analyzer.Write(value_uInt8, sizeof(value_uInt8));

    // Write collection entries count
    value_uInt32 := php_analyzer.dbfiles.Count;
    analyzer.Write(value_uInt32, sizeof(value_uInt32));

    // Write entries
    for entry1 := 0 to php_analyzer.dbfiles.Count-1 do
    begin
      // Write checksum
      analyzer.Write(php_analyzer.dbfiles.Items[entry1].crc, sizeof(php_analyzer.dbfiles.Items[entry1].crc));

      // Write path + file length
      value_uInt16 := Length(php_analyzer.dbfiles.Items[entry1].sFile);  // Possible precision loosing is ok here, since paths are no longer than 2^16 (they are probably even no longer than 2^8)
      analyzer.Write(value_uInt16, sizeof(value_uInt16));

      // Write path + file
      if (value_uInt16 > 0) then
        analyzer.Write(php_analyzer.dbfiles.Items[entry1].sFile[1], value_uInt16 * sizeof(char));
    end;
  end;

  // Write class linking list
  if (php_analyzer.classLinkingList.Count > 0) then
  begin
    // Write collection ID
    value_uInt8 := 4;
    analyzer.Write(value_uInt8, sizeof(value_uInt8));

    // Write collection entries count
    value_uInt32 := php_analyzer.classLinkingList.Count;
    analyzer.Write(value_uInt32, sizeof(value_uInt32));

    // Write entries
    for entry1 := 0 to php_analyzer.classLinkingList.Count-1 do
    begin
      // Write index of class in class collection
      analyzer.Write(php_analyzer.classLinkingList.Items[entry1].classIndex, sizeof(php_analyzer.classLinkingList.Items[entry1].classIndex));

      // Connected files count
      analyzer.Write(php_analyzer.classLinkingList.Items[entry1].fileIndices.Count, sizeof(php_analyzer.classLinkingList.Items[entry1].fileIndices.Count));

      // Write connected file entries
      for entry2 := 0 to php_analyzer.classLinkingList.Items[entry1].fileIndices.Count-1 do
      begin
        // Write index of file in checksums collection
        value_Int32 := php_analyzer.classLinkingList.Items[entry1].fileIndices.Items[entry2];  // Delphi seems not accepting integer values from generics collections as variables...
        analyzer.Write(value_Int32, sizeof(value_Int32));
      end;
    end;
  end;

  // Release analyzer file
  analyzer.Free();

  inherited;
end;

procedure TCCCPlugin.BeNotified(sn: PSCNotification);
var
  docType: TNppLang;
  isCorrectDocType: Boolean;
  hCurrentEditView: HWND;
  iLength: Integer;
  iCurrentPos: Integer;
  WndMainRect: TRect;
  iCharPosX, iCharPosY: Integer;
  iLineHeight: Integer;
  iPopupLeft, iPopupTop: Integer;
  cpText: array of AnsiChar;
  sText, sOrigText: String;
  bUTF8: Boolean;
  iUTF8PosInUnicode: Integer;
  php_class: AnalyzerPHPClass;
  i, j, k: Integer;
  sItem: String;
  rWorkArea: TRect;
  c: Char;
begin
  inherited;

  if bInitialized then
  begin
    SendMessage(self.NppData.NppHandle, NPPM_GETCURRENTLANGTYPE, 0, LPARAM(@docType));
    isCorrectDocType := docType = L_PHP;  // isCorrectDocType := iif(docType = L_PHP, true, false);

    if
    (
      (isCorrectDocType) and
      (sn^.nmhdr.code = SCN_CHARADDED)
    )
    then
    begin
      hCurrentEditView := self.GetCurrentScintilla();
      iLength := SendMessage(hCurrentEditView, SCI_GETLENGTH, 0, 0);
      iCurrentPos := SendMessage(hCurrentEditView, SCI_GETCURRENTPOS, 0, 0);

      // Find scintilla window position & "char-cursor" position offset inside this window
      GetWindowRect(hCurrentEditView, WndMainRect);
      iCharPosX := SendMessage(hCurrentEditView, SCI_POINTXFROMPOSITION, 0, iCurrentPos);
      iCharPosY := SendMessage(hCurrentEditView, SCI_POINTYFROMPOSITION, 0, iCurrentPos);
      iLineHeight := SendMessage(hCurrentEditView, SCI_TEXTHEIGHT, 0, 0);

      iPopupLeft := WndMainRect.left + iCharPosX;
      iPopupTop := WndMainRect.top + iCharPosY;

      c := Char(sn^.ch);

      if
      (
        (
          (Char(sn^.ch) = '>') or
          (Char(sn^.ch) = ':')
        ) and
        (iLength > 2) and
        (accpc_settings_cbEnableAutocompletion.Checked)
      )
      then begin
        SetLength(cpText, iLength + 1);
        SendMessage(hCurrentEditView, SCI_GETTEXT, iLength + 1, LPARAM(PChar(cpText)));

        // Check if we have the full operator in front of the caret, otherwise leave procedure
        if
        (
          not (
            (cpText[iCurrentPos-2] = '-') and
            (cpText[iCurrentPos-1] = '>')
          )
          and
          not (
            (cpText[iCurrentPos-2] = ':') and
            (cpText[iCurrentPos-1] = ':')
          )
        )
        then begin
          Exit();
        end;

        // Get entire text
        if (IsUTF8String(PAnsiChar(cpText))) then
        begin
          bUTF8 := true;
          sText := UTF8ToString(cpText);
          sOrigText := sText;
        end
        else
        begin
          bUTF8 := false;
          sText := String(PAnsiChar(cpText));
          sOrigText := sText;
        end;

        // For testing...
        //Application.MessageBox(PWideChar(String('Is UTF8: ' + iif(bUTF8, 'true', 'false'))), '', 64);
        //Application.MessageBox(PWideChar(sText), '', 64);

        // Because SCI_GETCURRENTPOS returns the position measured in bytes and not chars,
        // the returned position can be wrong with UTF-8 characters (they might be 2, 3 or 4 bytes per char).
        // Estimate the correct position by char.
        iUTF8PosInUnicode := UTF8UnicodePos(PAnsiChar(cpText), iCurrentPos);

        SetLength(cpText, 0);  // This is not neccessary but it might give back some memory space (if text is very huge) until procedure finishes

        // Update working status
        php_analyzer.isAnalyzing := true;

        // Remove temporarily defined classes (which are defined in current Notepad++ session but not saved anywhere)
        // They will be re-assigned if they are still in the scintilla view.
        for i := php_analyzer.classLinkingList.Count-1 downto 0 do
        begin
          if
          (
            (php_analyzer.classLinkingList.Items[i].fileIndices.Count = 0) or
            (php_analyzer.classLinkingList.Items[i].fileIndices.Contains(-1))
          )
          then begin
            // Delete class and linking entry
            php_analyzer.classes.Delete(php_analyzer.classLinkingList.Items[i].classIndex);
            php_analyzer.classLinkingList.Delete(i);
          end;
        end;

        // Analyze files in root directory and it's subdirectories
        php_analyzer.Reset(not accpc_settings_cbResetEverytime.Checked);  // Parse all files regardless if parsed before & not changed? Note: Invert
        php_analyzer.RootDirectory := accpc_settings_leRootDirectory.Text;
        php_analyzer.bGetParamValues := true;
        if (Length(php_analyzer.RootDirectory) > 0) then
        begin
          php_analyzer.RootDirectory := IncludeTrailingPathDelimiter(php_analyzer.RootDirectory);
          if (DirectoryExists(php_analyzer.RootDirectory)) then
          begin
            // Parse all files
            php_analyzer.AnalyzeDirectory();
          end;
        end;

        // Parse text in current NPP view (since it might not have been saved somewhere)
        php_analyzer.Reset(false);
        php_analyzer.text := @sText;
        php_analyzer.AnalyzePHP('*current Notepad++ window*');  // The asterisk are not allowed in file names
        php_analyzer.Inherit();

        // Try to find the definition of current variable to know who's (of which class) attributes & methods to show in popup box
        php_analyzer.Reset(false);
        sText := sOrigText;
        php_analyzer.text := @sText;
        php_analyzer.iPos := iUTF8PosInUnicode + 1;  // Note: NPP counts from 0, VLC strings from 1 - so +1
        php_analyzer.iOldDirection := asdNone;
        php_analyzer.savePosition();  // Store position and direction, etc. in old values...
        php_analyzer.PrepareTextForParsing(true);  // Remove comments, strings, non PHP data, etc.
        php_class := GetCurrentPHPClass(Char(sn^.ch));

        //php_analyzer.dump('R:\test.txt', 2);

        // We need to pre-create the docking window if it was not already create
        // (e. g. when Notepad++ starts), because in the next step need to modify
        // the TVirtualStringTree in the docking window. This is done only once
        // (and only if not already created).
        if (not Assigned(ClassesListDockingForm)) then
        begin
          Npp.FuncClassList();  // Create
          Npp.FuncClassList();  // Hide
        end;

        // Rebuild list in PHP Class Inspector
        // Note: This is needed, since ClassesListDockingForm.ClassListTreeViewDrawText() is accessing the global analyzer object & we need to update it.
        ClassesListDockingForm.BuildClassLinkingList(php_analyzer);

        // Update working status
        php_analyzer.isAnalyzing := false;

        // If variable definition and class found, prepare popup and show it
        if (Assigned(php_class)) then
        begin
          // Fill popup box
          // Separator: |
          // Field 1: Name
          // Field 2: Attribute/Method
          // Field 3: Visibility (+,-,#)
          // Field 4: Static (s)
          // Field 5: Const (c)
          accpc_popup_listbox.Items.Clear();

          // Attributes
          for i := 0 to php_class.attributes.Count-1 do
          begin
            if
            (
              (accpc_settings_cbShowPrivate.Checked) or
              (
                (php_class.attributes.Items[i].visibility <> asvPrivate) and
                (php_class.attributes.Items[i].visibility <> asvProtected)
              )
            )
            then begin
              // Build - In order to use the default autocomplete functionality, place the name in front
              sItem := php_class.attributes.Items[i].name +'|';
              sItem := sItem + 'Attribute|';
              case php_class.attributes.Items[i].visibility of
                asvPublic: sItem := sItem + '+|';
                asvPrivate: sItem := sItem + '-|';
                asvProtected: sItem := sItem + '#|';
                else sItem := sItem + '?|';  // like asvUnknown
              end;
              sItem := sItem + iif(php_class.attributes.Items[i].bStatic, 's|', '|');
              sItem := sItem + iif(php_class.attributes.Items[i].bConst, 'c', '');

              if
              (
                (
                  (Char(sn^.ch) = '>') and
                  (not php_class.attributes.Items[i].bStatic) and
                  (not php_class.attributes.Items[i].bConst)
                )
                or
                (
                  (Char(sn^.ch) = ':') and
                  (
                    (php_class.attributes.Items[i].bStatic) or
                    (php_class.attributes.Items[i].bConst)
                  )
                )
              )
              then begin
                accpc_popup_listbox.Items.Add(sItem);
              end;
            end;
          end;

          // Methods
          for i := 0 to php_class.methods.Count-1 do
          begin
            if
            (
              (accpc_settings_cbShowPrivate.Checked) or
              (
                (php_class.methods.Items[i].visibility <> asvPrivate) and
                (php_class.methods.Items[i].visibility <> asvProtected)
              )
            )
            then begin
              sItem := php_class.methods.Items[i].name;

              // Parameters
              sItem := sItem + '(';
              k := php_class.methods.Items[i].parameters.Count;
              for j := 0 to k-1 do
              begin
                sItem := sItem + php_class.methods.Items[i].parameters.Strings[j];
                if (j < k-1) then
                begin
                  sItem := sItem + ',';
                end;
              end;
              sItem := sItem + ')|';

              sItem := sItem + 'Method|';

              case php_class.methods.Items[i].visibility of
                asvPublic: sItem := sItem + '+|';
                asvPrivate: sItem := sItem + '-|';
                asvProtected: sItem := sItem + '#|';
                else sItem := sItem + '?|';  // like asvUnknown
              end;
              sItem := sItem + iif(php_class.methods.Items[i].bStatic, 's|', '|');

              if
              (
                (
                  //(Char(sn^.ch) = '>') and  // [SE20140406011800]: I get a weird "unknown exception" at runtime here although this is not even executed at startup. Hard to reproduce. Is it some kind of Delphi compiler optimization?
                  (c = '>') and
                  (not php_class.methods.Items[i].bStatic)
                )
                or
                (
                  //(Char(sn^.ch) = ':') and
                  (c = ':') and
                  (php_class.methods.Items[i].bStatic)
                )
              )
              then begin
                accpc_popup_listbox.Items.Add(sItem);
              end;
            end;
          end;

          // Set popup's window position
          accpc_popup.Left := iPopupLeft;
          accpc_popup.Top := iPopupTop + iLineHeight;

          // Move popup's window up and/or left if it's bottom and/or left borders go beyond screen width/height.
          // Note: Screen width/height must not be the same as the width/height of working area. Disregard taskbar.
          SystemParametersInfo(SPI_GETWORKAREA, 0, rWorkArea, 0);
          if (accpc_popup.Top + accpc_popup.Height > rWorkArea.Bottom) then
          begin
            accpc_popup.Top := accpc_popup.Top - (accpc_popup.Height + iLineHeight);
          end;
          if (accpc_popup.Left + accpc_popup.Width > rWorkArea.Right - 30) then  // Don't overlap NPP's scrollbar
          begin
            accpc_popup.Left := accpc_popup.Left - accpc_popup.Width;
          end;

          accpc_popup.ShowModal();
        end;
      end;
    end;
  end;
end;

procedure TCCCPlugin.BeforeDestruction;
begin
  inherited;
end;

procedure _FuncSettings; cdecl;
begin
  Npp.FuncSettings;
end;

procedure _FuncClassList; cdecl;
begin
  Npp.FuncClassList;
end;

procedure _FuncAbout; cdecl;
begin
  Npp.FuncAbout;
end;

procedure TCCCPlugin.FuncSettings;
Const
  settingsStringSize = 1024;
var
  iOKLoad: Integer;
  settingsString: array[0..settingsStringSize] of WideChar;
  settingsVersion: UINT32;
begin
  // Save settings if save button clicked
  accpc_settings_var_oldleRootDirectory := accpc_settings_leRootDirectory.Text;
  if (accpc_settings.ShowModal() = mrOk) then
  begin
    WritePrivateProfileString('settings', 'version', PWideChar(sAppVersion), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'EnableAutocompletion', PWideChar(String(iif(accpc_settings_cbEnableAutocompletion.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'ShowPrivate', PWideChar(String(iif(accpc_settings_cbShowPrivate.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'ShowParamTypeCasting', PWideChar(String(iif(accpc_settings_cbShowParamTypeCasting.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'ResetEverytime', PWideChar(String(iif(accpc_settings_cbResetEverytime.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'RootDirectory', PWideChar(accpc_settings_leRootDirectory.Text), PWideChar(settingsPath + settingsFile));
    WritePrivateProfileString('settings', 'AutocompletionTimeout', PWideChar(IntToStr(accpc_settings_tbAutocompletionTimeout.Position)), PWideChar(settingsPath + settingsFile));
    php_analyzer.RootDirectory := accpc_settings_leRootDirectory.Text;
    php_analyzer.bGetParamTypeCasting := accpc_settings_cbShowParamTypeCasting.Checked;

    // Hide PHP Class Inspector if plugin disabled
    if
    (
      (Assigned(ClassesListDockingForm)) and
      (not accpc_settings_cbEnableAutocompletion.Checked) and
      (ClassesListDockingForm.DockingFormActive)
    )
    then ClassesListDockingForm.Hide();

    // Reset AnalyzerPHP object if path changed
    if (accpc_settings_leRootDirectory.Text <> accpc_settings_var_oldleRootDirectory) then
    begin
      php_analyzer.Reset(true);
    end;
  end
  else
  begin
    // Re-load settings (restore them - do it here so PHP analyzer can access actual values of components directly)
    iOKLoad := IDYES;
    settingsVersion := GetPrivateProfileInt('settings', 'version', iAppVersion, PWideChar(settingsPath + settingsFile));
    if not settingsCompatibleVersions.Contains(settingsVersion) then
    begin
      iOKLoad := Application.MessageBox('Settings file seems to be incompatible.'+ #10 +'Loading it may mess things up.'+ #10#10 +'Do you want to try to load it?', 'ACCPC - Loading configuration file', 4+48);
    end;

    if (iOKLoad = IDYES) then
    begin
      accpc_settings_cbEnableAutocompletion.Checked := Boolean(GetPrivateProfileInt('settings', 'EnableAutocompletion', 1, PWideChar(settingsPath + settingsFile)));
      accpc_settings_cbShowPrivate.Checked := Boolean(GetPrivateProfileInt('settings', 'ShowPrivate', 0, PWideChar(settingsPath + settingsFile)));
      accpc_settings_cbShowParamTypeCasting.Checked := Boolean(GetPrivateProfileInt('settings', 'ShowParamTypeCasting', 0, PWideChar(settingsPath + settingsFile)));
      accpc_settings_cbResetEverytime.Checked := Boolean(GetPrivateProfileInt('settings', 'ResetEverytime', 1, PWideChar(settingsPath + settingsFile)));
      accpc_settings_tbAutocompletionTimeout.Position := GetPrivateProfileInt('settings', 'AutocompletionTimeout', 10, PWideChar(settingsPath + settingsFile));
      GetPrivateProfileString('settings', 'RootDirectory', '', settingsString, settingsStringSize-1, PWideChar(settingsPath + settingsFile));
      accpc_settings_leRootDirectory.Text := String(settingsString);
      php_analyzer.RootDirectory := String(settingsString);
      php_analyzer.bGetParamTypeCasting := accpc_settings_cbShowParamTypeCasting.Checked;
    end;
  end;
end;

procedure TCCCPlugin.FuncClassList;
begin
  if (not Assigned(ClassesListDockingForm)) then
  begin
    ClassesListDockingForm := TClassesListDockingForm.Create(self, 1);
  end
  else
  begin
    if (ClassesListDockingForm.DockingFormActive) then
    begin
      ClassesListDockingForm.Hide();
    end
    else
    begin
      ClassesListDockingForm.Show();
    end;
  end;
end;

procedure TCCCPlugin.FuncAbout;
var
  aboutForm: TAboutForm;
begin
  aboutForm := TAboutForm.Create(self);
  aboutForm.Label2.Caption := 'Version '+ sAppVersionDisplay +' (Win32, Unicode) for Notepad++';
  aboutForm.ShowModal;
  aboutForm.Free;
end;

procedure TCCCPlugin.DoNppnToolbarModification;
var
  tb: TToolbarIcons;
begin
  tb.ToolbarIcon := 0;
  tb.ToolbarBmp := LoadImage(Hinstance, 'TB_PHPCL', IMAGE_BITMAP, 0, 0, (LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS or LR_LOADTRANSPARENT));
  SendMessage(self.NppData.NppHandle, NPPM_ADDTOOLBARICON, WPARAM(self.CmdIdFromDlgId(1)), LPARAM(@tb));
end;

procedure TCCCPlugin.DoNppnReady;
Const
  settingsStringSize = 1024;
var
  iOKLoad: Integer;
  settingsString: array[0..settingsStringSize] of WideChar;
  settingsVersion: UINT32;

  analyzer: TFileStream;
  value_uInt8: UINT8;
  value_uInt16: UINT16;
  value_uInt32: UINT32;
  value_Int32: INT32;
  value_string: String;
  entry1: INT32;
  entry2: INT32;
  collectionEntriesCount: INT32;
  classAttributesCount: INT32;
  classMethodsCount: INT32;
begin

  // Get path for configuration & other files
  analyzerFile := 'accpc_analyzer.dat';
  settingsFile := 'accpc.ini';
  settingsPath := self.GetPluginsConfigDir();

  if not DirectoryExists(settingsPath) then
  begin
    ForceDirectories(settingsPath);
  end;
  settingsPath := IncludeTrailingPathDelimiter(settingsPath);

  // Pre-load settings
  iOKLoad := IDYES;
  settingsVersion := GetPrivateProfileInt('settings', 'version', iAppVersion, PWideChar(settingsPath + settingsFile));
  if not settingsCompatibleVersions.Contains(settingsVersion) then
  begin
    iOKLoad := Application.MessageBox('Settings file seems to be incompatible.'+ #10 + 'Loading it may mess things up.'+ #10#10 +'Do you want to try to load it?', 'ACCPC - Loading configuration file', 4+48);
  end;

  if (iOKLoad = IDYES) then
  begin
    Application.ProcessMessages();  // Without this the next following objects's attribute (ClientWidth) is not set correctly for some reason (14 is substracted from the loaded value on my system - it might be the width of a window border * 2 => find out)
    accpc_popup.ClientWidth := GetPrivateProfileInt('settings', 'ClientWidth', accpc_popup.ClientWidth, PWideChar(settingsPath + settingsFile));
    accpc_popup.ClientHeight := GetPrivateProfileInt('settings', 'ClientHeight', accpc_popup.ClientHeight, PWideChar(settingsPath + settingsFile));
    accpc_settings_cbEnableAutocompletion.Checked := Boolean(GetPrivateProfileInt('settings', 'EnableAutocompletion', 1, PWideChar(settingsPath + settingsFile)));
    accpc_settings_cbShowPrivate.Checked := Boolean(GetPrivateProfileInt('settings', 'ShowPrivate', 0, PWideChar(settingsPath + settingsFile)));
    accpc_settings_cbShowParamTypeCasting.Checked := Boolean(GetPrivateProfileInt('settings', 'ShowParamTypeCasting', 0, PWideChar(settingsPath + settingsFile)));
    accpc_settings_cbResetEverytime.Checked := Boolean(GetPrivateProfileInt('settings', 'ResetEverytime', 1, PWideChar(settingsPath + settingsFile)));
    accpc_settings_tbAutocompletionTimeout.Position := GetPrivateProfileInt('settings', 'AutocompletionTimeout', 10, PWideChar(settingsPath + settingsFile));
    GetPrivateProfileString('settings', 'RootDirectory', '', settingsString, settingsStringSize-1, PWideChar(settingsPath + settingsFile));
    accpc_settings_leRootDirectory.Text := String(settingsString);
    php_analyzer.RootDirectory := String(settingsString);
    php_analyzer.bGetParamTypeCasting := accpc_settings_cbShowParamTypeCasting.Checked;
  end;

  // Load PHP analyzer object
  if FileExists(settingsPath + analyzerFile) then
  begin
    analyzer := TFileStream.Create(settingsPath + analyzerFile, fmOpenRead);

    // Read analyzer file version
    analyzer.Read(value_uInt32, sizeof(value_uInt32));

    // Check version. Simply skip loading if versions do not match (in that case the AnalyzerPHP object will be rebuilt automatically)
    if value_uInt32 = iAppVersion then
    begin
      while analyzer.Position < analyzer.Size-1 do
      begin

        // Read collection ID
        analyzer.Read(value_uInt8, sizeof(value_uInt8));

        // Read collection entries count
        analyzer.Read(collectionEntriesCount, sizeof(collectionEntriesCount));

        // Read classes
        if (value_uInt8 = 1) then
        begin
          for entry1 := 0 to collectionEntriesCount-1 do
          begin
            // Create class
            php_analyzer.classes.Add(AnalyzerPHPClass.Create());

            // Read class name length
            analyzer.Read(value_uInt16, sizeof(value_uInt16));

            // Read class name
            SetLength(value_string, value_uInt16);
            analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
            php_analyzer.classes.Last.name := value_string;

            // Read derived class name length
            analyzer.Read(value_uInt16, sizeof(value_uInt16));

            // Read derived class name
            SetLength(value_string, value_uInt16);
            analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
            php_analyzer.classes.Last.derivedFromClass := value_string;

            // Read offset position in file
            analyzer.Read(value_uInt32, sizeof(value_uInt32));
            php_analyzer.classes.Last.fileOffset := value_uInt32;

            // Read attributes count
            analyzer.Read(classAttributesCount, sizeof(classAttributesCount));

            // Read attributes
            for entry2 := 0 to classAttributesCount-1 do
            begin
              // Create attribute
              php_analyzer.classes.Last.attributes.Add(AnalyzerPHPAttribute.Create());

              // Read visibility
              analyzer.Read(value_uInt8, sizeof(value_uInt8));
              case value_uInt8 of
                1: php_analyzer.classes.Last.attributes.Last.visibility := asvPublic;
                2: php_analyzer.classes.Last.attributes.Last.visibility := asvPrivate;
                3: php_analyzer.classes.Last.attributes.Last.visibility := asvProtected;
                else php_analyzer.classes.Last.attributes.Last.visibility := asvUnknown;
              end;

              // Read static
              analyzer.Read(value_uInt8, sizeof(value_uInt8));
              php_analyzer.classes.Last.attributes.Last.bStatic := Boolean(value_uInt8);

              // Read const
              analyzer.Read(value_uInt8, sizeof(value_uInt8));
              php_analyzer.classes.Last.attributes.Last.bConst := Boolean(value_uInt8);

              // Read attribute name length
              analyzer.Read(value_uInt16, sizeof(value_uInt16));

              // Read attribute name
              SetLength(value_string, value_uInt16);
              analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
              php_analyzer.classes.Last.attributes.Last.name := value_string;

              // Read offset position in file
              analyzer.Read(value_uInt32, sizeof(value_uInt32));
              php_analyzer.classes.Last.attributes.Last.fileOffset := value_uInt32;
            end;

            // Read methods count
            analyzer.Read(classMethodsCount, sizeof(classMethodsCount));

            // Read methodes
            for entry2 := 0 to classMethodsCount-1 do
            begin
              // Create method
              php_analyzer.classes.Last.methods.Add(AnalyzerPHPMethod.Create());

              // Read visibility
              analyzer.Read(value_uInt8, sizeof(value_uInt8));
              case value_uInt8 of
                1: php_analyzer.classes.Last.methods.Last.visibility := asvPublic;
                2: php_analyzer.classes.Last.methods.Last.visibility := asvPrivate;
                3: php_analyzer.classes.Last.methods.Last.visibility := asvProtected;
                else php_analyzer.classes.Last.methods.Last.visibility := asvUnknown;
              end;

              // Read static
              analyzer.Read(value_uInt8, sizeof(value_uInt8));
              php_analyzer.classes.Last.methods.Last.bStatic := Boolean(value_uInt8);

              // Read method name length
              analyzer.Read(value_uInt16, sizeof(value_uInt16));

              // Read attribute name
              SetLength(value_string, value_uInt16);
              analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
              php_analyzer.classes.Last.methods.Last.name := value_string;

              // Read parameters text length
              analyzer.Read(value_uInt32, sizeof(value_uInt32));

              // Read parameters text
              SetLength(value_string, value_uInt32);
              analyzer.Read(value_string[1], value_uInt32 * sizeof(char));
              php_analyzer.classes.Last.methods.Last.parameters.Text := value_string;

              // Read offset position in file
              analyzer.Read(value_uInt32, sizeof(value_uInt32));
              php_analyzer.classes.Last.methods.Last.fileOffset := value_uInt32;
            end;
          end;
        end

        // Read constants
        else if (value_uInt8 = 2) then
        begin
          for entry1 := 0 to collectionEntriesCount-1 do
          begin
            // Create constant
            php_analyzer.constants.Add(AnalyzerPHPConstant.Create());

            // Read constant name length
            analyzer.Read(value_uInt16, sizeof(value_uInt16));

            // Read constant name
            SetLength(value_string, value_uInt16);
            analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
            php_analyzer.constants.Last.name := value_string;

            // Read constant value length
            analyzer.Read(value_uInt16, sizeof(value_uInt16));

            // Read constant value
            SetLength(value_string, value_uInt16);
            analyzer.Read(value_string[1], value_uInt16 * sizeof(char));
            php_analyzer.constants.Last.value := value_string;

            // Read case sensitivity
            analyzer.Read(value_uInt8, sizeof(value_uInt8));
            php_analyzer.constants.Last.case_sensitive := Boolean(value_uInt8);
          end;
        end

        // Read checksums
        else if (value_uInt8 = 3) then
        begin
          for entry1 := 0 to collectionEntriesCount-1 do
          begin
            // Read checksum
            analyzer.Read(value_uInt32, sizeof(value_uInt32));

            // Read length of path + file name
            analyzer.Read(value_uInt16, sizeof(value_uInt16));

            // Read path + file name
            // Note: We don't need to take care of character length and so on, since VCL strings are fixed 2 byte chars. We will read the data direct into the variable.
            SetLength(value_string, value_uInt16);
            analyzer.Read(value_string[1], value_uInt16 * sizeof(char));  // Read directly into VCL string

            // Create checksum DB entry
            php_analyzer.dbfiles.Add(crcdb.Create());
            php_analyzer.dbfiles.Last.crc := value_uInt32;
            php_analyzer.dbfiles.Last.sFile := value_string;
          end;
        end

        // Read class linking list
        else if (value_uInt8 = 4) then
        begin
          for entry1 := 0 to collectionEntriesCount-1 do
          begin
            // Create class linking list entry
            php_analyzer.classLinkingList.Add(ClassLinkingListEntry.Create());

            // Read index of class in class collection
            analyzer.Read(value_uInt32, sizeof(value_uInt32));
            php_analyzer.classLinkingList.Last.classIndex := value_uInt32;

            // Read connected files count
            analyzer.Read(value_uInt32, sizeof(value_uInt32));

            // Read connected files
            for entry2 := 0 to value_uInt32-1 do
            begin
              // Read index of file in checksums collection & add to list
              analyzer.Read(value_Int32, sizeof(value_Int32));
              php_analyzer.classLinkingList.Last.fileIndices.Add(value_Int32);
            end;
          end;
        end;

      end;
    end;

    // Release analyzer file
    analyzer.Free();
  end;

  bInitialized := true;
end;

initialization
  Npp := TCCCPlugin.Create;
end.
