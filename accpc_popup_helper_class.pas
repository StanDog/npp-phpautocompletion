//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit accpc_popup_helper_class;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, shellapi, SciSupport, Vcl.FileCtrl, System.AnsiStrings,
  ParsingIndicatorForms, Vcl.Imaging.pngimage;

type
  Taccpc_popup_helper_class = class
  public
    procedure foregroundTimerOnTimer(Sender: TObject);
    procedure chooseRootDirClick(Sender: TObject);
    procedure popupFormResize(Sender: TObject);
    procedure popupListboxDblClick(Sender: TObject);
    procedure popupListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure popupListboxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure settingsFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure settingsFormShow(Sender: TObject);
    procedure popupFormShow(Sender: TObject);
    procedure autocompletionTimeoutChange(Sender: TObject);
  end;

implementation

uses cccplugin, ccchelperfunctions;

// Note: This is a dirty fix to bring windows with fsStayOnTop flag back to front
// after the main Notepad++ window was activated (for some reason, windows with that
// flag do not stay on top...). It would be better to find a way to react to Notepad++'s
// main window, when it gets de-/activated.
procedure Taccpc_popup_helper_class.foregroundTimerOnTimer(Sender: TObject);
var
  hWndForeground: HWND;
  className: array[0..95] of WideChar;
begin
  hWndForeground := GetForegroundWindow();

  if (hWndForeground <> 0) then
  begin
    GetClassNameW(hWndForeground, className, 95);
    if String(className) = 'Notepad++' then
    begin
      // Bring popup window to front if it is opened
      if accpc_popup.Visible then
        accpc_popup.BringToFront();

      // Bring settings window to front if it is opened
      if accpc_settings.Visible then
        accpc_settings.BringToFront();

      // Bring progress indicator window to front if it is opened
      if
      (
        (Assigned(ParsingIndicatorForm)) and
        (ParsingIndicatorForm.Visible)
      )
      then
        ParsingIndicatorForm.BringToFront();
    end;
  end;
end;

procedure Taccpc_popup_helper_class.chooseRootDirClick(Sender: TObject);
var
  sRootDir: String;
begin
  if SelectDirectory('Select root directory', 'Root directory', sRootDir, [sdNewFolder, sdShowEdit, sdNewUI, sdValidateDir]) then
  begin
    accpc_settings_leRootDirectory.Text := sRootDir;
  end;
  accpc_settings.BringToFront();
end;

procedure Taccpc_popup_helper_class.settingsFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
    if Key = VK_ESCAPE then
    begin
      accpc_settings.ModalResult := mrCancel;
      // Close();
    end;
end;

procedure Taccpc_popup_helper_class.settingsFormShow(Sender: TObject);
begin
   self.autocompletionTimeoutChange(Sender);
end;

procedure Taccpc_popup_helper_class.popupFormShow(Sender: TObject);
begin
  accpc_popup_listbox.AutoCompleteDelay := accpc_settings_tbAutocompletionTimeout.Position * 100;
end;

procedure Taccpc_popup_helper_class.popupFormResize(Sender: TObject);
begin
  accpc_popup_listbox.Invalidate();
end;

procedure Taccpc_popup_helper_class.popupListboxDblClick(Sender: TObject);
var
  Key: Word;
  Shift: TShiftState;
begin
  Key := VK_RETURN;
  Npp.accpc_popup_helper_class.popupListboxKeyDown(Sender, Key, Shift);
end;

procedure Taccpc_popup_helper_class.popupListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  list: TListBox;
  item: String;
  sName: String;
  sType: String;  // String "Attribute" or "Method"
  bStatic: Boolean;
  sParameterString: String;
  parameters: TStringList;
  bParametersFound: Boolean;
  i: Integer;
  hCurrentEditView: HWND;
  iCurrentPos: Integer;
  sInsert: Utf8String;
begin
  list := Sender as TListBox;
  bParametersFound := false;

  if (Key = VK_RETURN) then
  begin
    if (list.ItemIndex <> -1) then
    begin
      item := list.Items.Strings[list.ItemIndex];

      // Get name of method / attribute
      sName := ExtractFieldContents(item, 0, '|');
      sType := ExtractFieldContents(item, 1, '|');
      bStatic := (ExtractFieldContents(item, 3, '|') = 's');

      // Get parameters
      if Pos('(', sName) > 0 then
      begin
        // Get parameters
        parameters := TStringList.Create();
        sParameterString := sName;
        Delete(sParameterString, 1, Pos('(', sParameterString));
        sParameterString := Copy(sParameterString, 1, sParameterString.Length-1);
        while (Pos(',', sParameterString) > 0) do
        begin
          parameters.Add(Copy(sParameterString, 1, Pos(',', sParameterString)-1));
          Delete(sParameterString, 1, Pos(',', sParameterString));
        end;
        if Length(Trim(sParameterString)) > 0 then
          parameters.Add(sParameterString);  // Push last back

        // Rebuild name
        sName := Copy(sName, 1, Pos('(', sName));
        bParametersFound := false;
        for i := 0 to parameters.Count-1 do
        begin
          if Pos('=', parameters[i]) = 0 then
          begin
            if bParametersFound and (i > 0) then
            begin
              sName := sName + ', ';
            end;

            //sName = sName + parameters[i];  // Note: Don't place parameter names (display them in popup only)
            bParametersFound := true;
          end
          else
          begin
            //bParametersFound := false;
            break;
          end;
        end;
        sName := sName + ')';
      end;

      // If entry is a static attribute, prepend "$"
      if
      (
        (sType = 'Attribute') and
        (bStatic)
      )
      then begin
        sName := '$' + sName;
      end;


      // Convert WideChar / UCS2 / UTF-16 to UTF-8
      sInsert := Utf8Encode(sName);

      // Get handle
      hCurrentEditView := Npp.GetCurrentScintilla();

      // Get old position & place new position directly into function call (inside brackets)
      // Note: Since we don't need the position for any VCL strings, don't increment it.
      iCurrentPos := Integer(SendMessage(hCurrentEditView, SciSupport.SCI_GETCURRENTPOS, 0, 0));
      if System.AnsiStrings.PosEx(AnsiString('('), PAnsiChar(sInsert)) > 0 then
      begin
        if bParametersFound then
        begin
          iCurrentPos := iCurrentPos + System.AnsiStrings.PosEx(AnsiString('('), PAnsiChar(sInsert));  // Scintilla starts at 0, VCL at 1 -> position of bracket + 1 of VCL
        end
        else
        begin
          iCurrentPos := iCurrentPos + System.AnsiStrings.PosEx(AnsiString(')'), PAnsiChar(sInsert));  // Place behind brackets if no parameters
        end;
      end
      else
      begin
        iCurrentPos := iCurrentPos + Length(sInsert);
      end;

      // Insert text into NPP (scintilla) & place caret inside function call
      SendMessage(hCurrentEditView, SCI_BEGINUNDOACTION, 0, 0);
      SendMessage(hCurrentEditView, SCI_REPLACESEL, 0, LPARAM(PAnsiChar(sInsert)));
      SendMessage(hCurrentEditView, SCI_SETSEL, iCurrentPos, iCurrentPos);
      SendMessage(hCurrentEditView, SCI_ENDUNDOACTION, 0, 0);

      accpc_popup.Close();
    end;
  end
  else if
  (
    (Key = VK_ESCAPE) or
    (Key = VK_BACK) or
    (Key = VK_DELETE)
  )
  then begin
    accpc_popup.Close();
  end;
end;

procedure Taccpc_popup_helper_class.popupListboxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  list: TListBox;
  item: String;
  sName, sDisplayedName, sMethodAttribute, sVisibility, sStatic, sConst: String;
  parameters: TStringList;
  sParameterString: String;
  iShiftLeft: Integer;
  i: Integer;
  iDisplayedShiftX: Integer;
  iconVisibility: TPngImage;
begin
  list := Control as TListBox;
  item := list.Items.Strings[Index];

  sName := Copy(item, 1, Pos('|', item)-1);
  Delete(item, 1, Pos('|', item));
  sMethodAttribute := Copy(item, 1, Pos('|', item)-1);
  Delete(item, 1, Pos('|', item));
  sVisibility := Copy(item, 1, Pos('|', item)-1);
  Delete(item, 1, Pos('|', item));
  sStatic := Copy(item, 1, Pos('|', item)-1);
  Delete(item, 1, Pos('|', item));
  sConst := item;

  // Fill (erase) background
  list.Canvas.Pen.Style := psDot;
  list.Canvas.Pen.Color := clWindowText;
  list.Canvas.Brush.Style := bsSolid;
  list.Canvas.Brush.Color := iif(odSelected in State, clHighlight, clWindow);
  list.Canvas.FillRect(Rect);

  // Draw name of attribute / method (+ parameters)
  list.Canvas.Brush.Style := bsClear;
  if Pos('(', sName) > 0 then
  begin
    // Get parameters
    parameters := TStringList.Create();
    sParameterString := sName;
    Delete(sParameterString, 1, Pos('(', sParameterString));
    sParameterString := Copy(sParameterString, 1, sParameterString.Length-1);
    while (Pos(',', sParameterString) > 0) do
    begin
      parameters.Add(Copy(sParameterString, 1, Pos(',', sParameterString)-1));
      Delete(sParameterString, 1, Pos(',', sParameterString));
    end;
    parameters.Add(sParameterString);  // Push last back

    // If method name or parameters contain right-to-left chars, calculate displayed text width
    if TextIsRightToLeft(sName) then
    begin
      sDisplayedName := GetFormattedMethodString(sName);

      // Calculate displayed text width
      iDisplayedShiftX := ((Rect.Right - 26) - (Rect.Left + 3 + 65)) - list.Canvas.TextWidth(sDisplayedName);
    end
    else
    begin
      iDisplayedShiftX := 0;
    end;

    // Draw brackets with parameters
    iShiftLeft := Rect.Left + 3 + 65 + iDisplayedShiftX;
    list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
    list.Canvas.TextOut(iShiftLeft, Rect.Top + 1, Copy(sName, 1, Pos('(', sName)));  // Draw with bracket
    iShiftLeft := iShiftLeft + list.Canvas.TextWidth(Copy(sName, 1, Pos('(', sName)));

    for i := 0 to parameters.Count-1 do
    begin
      if Pos('=', parameters.Strings[i]) > 0 then
      begin
        list.Canvas.Font.Color := iif(odSelected in State, clSilver, clGray);
        list.Canvas.TextOut(iShiftLeft, Rect.Top + 1, TrimRight(Copy(parameters.Strings[i], 1, Pos('=', parameters.Strings[i])-1)));
        iShiftLeft := iShiftLeft + list.Canvas.TextWidth(TrimRight(Copy(parameters.Strings[i], 1, Pos('=', parameters.Strings[i])-1)));
      end
      else
      begin
        list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
        list.Canvas.TextOut(iShiftLeft, Rect.Top + 1, parameters.Strings[i]);
        iShiftLeft := iShiftLeft + list.Canvas.TextWidth(parameters.Strings[i]);
      end;

      if (i < parameters.Count-1) then
      begin
        list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
        list.Canvas.TextOut(iShiftLeft, Rect.Top + 1, ', ');
        iShiftLeft := iShiftLeft + list.Canvas.TextWidth(', ');
      end;
    end;

    // Draw last bracket
    list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
    list.Canvas.TextOut(iShiftLeft, Rect.Top + 1, ')');  // Draw with bracket
  end
  else
  begin
    if TextIsRightToLeft(sName) then
    begin
      iDisplayedShiftX := ((Rect.Right - 26) - (Rect.Left + 3 + 65)) - list.Canvas.TextWidth(sName);
    end
    else
      iDisplayedShiftX := 0;

    list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
    list.Canvas.TextOut(Rect.Left + 3 + 65 + iDisplayedShiftX, Rect.Top + 1, sName);
  end;

  // Fill (erase) left part + some pixels at left
  // Do this after drawing the name of attribute / method because of right-to-left texts
  list.Canvas.Brush.Style := bsSolid;
  list.Canvas.Brush.Color := iif(odSelected in State, clHighlight, clWindow);
  list.Canvas.FillRect(TRect.Create(Rect.Left, Rect.Top, Rect.Left + 3 + 65, Rect.Bottom));

  // Draw icon
  iconVisibility := TPngImage.Create();
  if (sVisibility = '+') then iconVisibility.LoadFromResourceName(hInstance, 'AC_VISIBILITY_PUBLIC')
  else if (sVisibility = '-') then iconVisibility.LoadFromResourceName(hInstance, 'AC_VISIBILITY_PRIVATE')
  else if (sVisibility = '#') then iconVisibility.LoadFromResourceName(hInstance, 'AC_VISIBILITY_PROTECTED');
  list.Canvas.Draw(Rect.Left + 3, Rect.Top + 4, iconVisibility);
  iconVisibility.Free();

  // Draw type (method / attribute)
  list.Canvas.Brush.Style := bsClear;
  list.Canvas.Font.Color := iif(odSelected in State, clYellow, clOlive);
  list.Canvas.TextOut(Rect.Left + 14, Rect.Top + 1, sMethodAttribute);

  // Fill (erase) right part + some pixels at left
  list.Canvas.Brush.Style := bsSolid;
  list.Canvas.Brush.Color := iif(odSelected in State, clHighlight, clWindow);
  list.Canvas.FillRect(TRect.Create(Rect.Right - 26, Rect.Top, Rect.Right, Rect.Bottom));
  list.Canvas.Brush.Style := bsClear;

  // Static
  list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
  list.Canvas.TextOut(Rect.Right - 24, Rect.Top + 1, iif(sStatic.Length > 0, 's', ''));

  // Const
  list.Canvas.Font.Color := iif(odSelected in State, clHighlightText, clWindowText);
  list.Canvas.TextOut(Rect.Right - 12, Rect.Top + 1, iif(sConst.Length > 0, 'c', ''));
end;

procedure Taccpc_popup_helper_class.autocompletionTimeoutChange(Sender: TObject);
begin
  accpc_settings_lblAutocompletionTimeoutValue.Caption := FormatFloat('0.0', accpc_settings_tbAutocompletionTimeout.Position / 10) + ' sec';
end;

end.
