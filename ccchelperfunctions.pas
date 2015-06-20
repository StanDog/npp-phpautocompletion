//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit ccchelperfunctions;

interface

uses
  AnalyzerPHP, SysUtils, System.Generics.Collections, Vcl.Graphics, Windows,
  Vcl.Forms, Vcl.Controls, Vcl.ImgList;

function iif(Test: boolean; TrueR, FalseR: variant): variant;
//function UTF8UnicodePos(str: array of AnsiChar; pos: Integer): Integer;
function UTF8UnicodePos(str: PAnsiChar; pos: Integer): Integer;
function GetCurrentPHPClass(c: Char): AnalyzerPHPClass;
function TextIsRightToLeft(text: String): Boolean;
function ExtractFieldContents(str: String; field: Integer; delimiter: String = ';'): String;
procedure ReplaceTextRange(var sText: String; iStart: Integer; iEnd: Integer; c: Char);
procedure Dump(sText: String; sDestFile: String);
procedure DrawGlassText(canvas: TCanvas; GlowSize: Integer; Text: UnicodeString; left, top, width, height: Integer; Format: DWORD; fontName: String; fontSize: Integer; fontStyles: TFontStyles = []; textColor: TColor = clWindowText); overload;
procedure DrawGlassText(Canvas: TCanvas; GlowSize: Integer; var Rect: TRect; var Text: UnicodeString; Format: DWORD); overload;
procedure ILAddMasked(var ImgList: Vcl.Controls.TImageList; ResourceName: String; TransparentColor: TColor);
function GetFormattedMethodString(str: String): String;

implementation

uses
  cccplugin, System.Classes, Types, UxTheme, Vcl.Themes;

//---------------------------------------------------------------------------
function iif(Test: boolean; TrueR, FalseR: variant): variant;
begin
 if Test then
  Result := TrueR
 else
  Result := FalseR;
end;
//---------------------------------------------------------------------------
//function UTF8UnicodePos(str: array of AnsiChar; pos: Integer): Integer;
function UTF8UnicodePos(str: PAnsiChar; pos: Integer): Integer;
begin
  str[pos] := #0;
  Result := Length(UTF8ToString(str));
end;
//---------------------------------------------------------------------------
function GetCurrentPHPClass(c: Char): AnalyzerPHPClass;
var
  sParent: String;
  iPointerPos: Integer;
  bOK: Boolean;
  sClassDeclaration: String;
  sClassDefinition: String;
  sTmp: String;
  iClass: Integer;
begin

  (*
    Note about: static / const
    ---------------------
    class Foo {}

    Foo::[$]constantOrStaticAttributeOrMethod		// On non instantiated class (no $ for constants)

    $class = new Foo();
    $class::[$]constantOrStaticAttributeOrMethod		// On instantiated class (no $ for constants)

    $classname = 'Foo';
    $classname::[$]constantOrStaticAttributeOrMethod	// On non instantiated "variable" class (as of PHP 5.3.0) (no $ for constants)

    ----
    Notes:
    - statics are accessed in PHP strict mode only through :: operator, not -> operator!
    - currently the indirect style (as of PHP 5.3.0) is not supported
  *)

  // Get previous object or "->" / "::"
  sParent := php_analyzer.getPrevious(false);

  // If pointer is with leading & following spaces
  if
  (
    (
      (c = '>') and
      (sParent = '->')
    )
    or
    (
      (c = ':') and
      (sParent = '::')
    )
  )
  then
  begin
    sParent := php_analyzer.getPrevious(false);
  end;

  // Parent may still contain "->" or "::"
  iPointerPos := 0;
  if (c = '>') then
  begin
    iPointerPos := Pos('->', sParent);
  end
  else if (c = ':') then
  begin
    iPointerPos := Pos('::', sParent);
  end;

  if (iPointerPos > 0) then
  begin
    Delete(sParent, iPointerPos, (Length(sParent) - iPointerPos)+1);  // Remove till the end, since it is possible that something is already something behind operator
  end;

  sParent := LowerCase(sParent);
  bOK := false;

  // Got parent variable, now search for definition
  if
  (
    (Length(sParent) > 0) and
    (sParent[1] = '$')
  )
  then begin
    while (true) do
    begin
      sClassDeclaration := LowerCase(php_analyzer.getPrevious(false));
      sClassDefinition := '';
      bOK := false;

      // Beginning of file reached -> leave loop search
      if (sClassDeclaration = '') then
      begin
        break;
      end;

      // Store position
      php_analyzer.savePosition();

      // Note:
      // Not supported: ;$obj  (<- sorry, but variable must begin with a $)
      // Supported: $obj=  (<- substring until first = will be taken)

      // $obj=new
      if (Pos('=new', sClassDeclaration) > 0) then
      begin
        sClassDeclaration := Copy(sClassDeclaration, 1, Pos('=new', sClassDeclaration)-1);

        if (sClassDeclaration = sParent) then
        begin
          sClassDefinition := php_analyzer.getNext(false);  // Get class name
          bOK := true;
        end;
      end
      // $obj= new
      else if (Pos('=', sClassDeclaration) > 0) then
      begin
        sClassDeclaration := Copy(sClassDeclaration, 1, Pos('=', sClassDeclaration)-1);

        sTmp := php_analyzer.getNext(false);  // Get "new"
        if
        (
          (sClassDeclaration = sParent) and
          (LowerCase(sTmp) = 'new')
        )
        then begin
          sClassDefinition := php_analyzer.getNext(false);  // Get class name
          bOK := true;
        end;
      end
      // $obj = new
      // $obj =new
      else
      begin
        if (sClassDeclaration = sParent) then
        begin
          sTmp := php_analyzer.getNext(false);  // Get "new" or "=new"
          if (sTmp = '=') then
          begin
            sTmp := php_analyzer.getNext(false);  // Get "new"
            if (LowerCase(sTmp) = 'new') then
            begin
              sClassDefinition := php_analyzer.getNext(false);  // Get class name
              bOK := true;
            end;
          end
          else if (LowerCase(sTmp) = '=new') then
          begin
            sClassDefinition := php_analyzer.getNext(false);  // Get class name
            bOK := true;
          end;
        end;
      end;

      // Found definition
      if (bOK) then
      begin
        // Leave while() loop & search
        break;
      end
      else
      begin
        // Restore old Position & go ahead with search
        php_analyzer.restorePosition();
      end;
    end;

    // Definition (class name) may contain brackets and/or semicolon
    if (bOK) then
    begin
      // Brackets found?
      if (Pos('(', sClassDefinition) > 0) then
      begin
        sClassDefinition := Copy(sClassDefinition, 1, Pos('(', sClassDefinition)-1);
      end
      else if (Pos(';', sClassDefinition) > 0) then
      begin
        sClassDefinition := Copy(sClassDefinition, 1, Pos(';', sClassDefinition)-1);
      end;

      // Find class object by name
      sClassDefinition := LowerCase(sClassDefinition);
      for iClass := 0 to php_analyzer.classes.Count-1 do
      begin
        if (LowerCase(php_analyzer.classes.Items[iClass].name) = sClassDefinition) then
        begin
          Exit(php_analyzer.classes.Items[iClass]);
          break;  // not needed(?)
        end;
      end;
    end;
  end
  // If we use the "::" operator and the word does not contain the variable indicator ($), then expect it to be the class name (no instantiated object)
  else if
  (
    (c = ':') and
    (Length(sParent) > 0) and
    (sParent[1] <> '$')
  )
  then begin
    // Find class object by name
    for iClass := 0 to php_analyzer.classes.Count-1 do
    begin
      if (LowerCase(php_analyzer.classes.Items[iClass].name) = sParent) then
      begin
        Exit(php_analyzer.classes.Items[iClass]);
        break;  // not needed(?)
      end;
    end;
  end;

  Result := nil;
end;
//---------------------------------------------------------------------------
function TextIsRightToLeft(text: String): Boolean;
var
  rightToLeftChars: String;
begin
  {
    Arabic: &#1575;&#1576;&#1578;&#1579;&#1580;&#1581;&#1582;&#1583;&#1584;&#1585;&#1586;&#1587;&#1588;&#1589;&#1590;&#1591;&#1592;&#1593;&#1594;&#1601;&#1602;&#1603;&#1604;&#1605;&#1606;&#1607;&#1608;&#1610;&#1577;
    Hebrew: &#1488;&#64305;&#1489;&#1490;&#1491;&#1492;&#1493;&#1494;&#1495;&#1496;&#1497;&#64315;&#1499;&#1498;&#1500;&#1502;&#1501;&#1504;&#1503;&#1505;&#1506;&#64324;&#1508;&#1507;&#1510;&#1509;&#1511;&#1512;&#64298;&#64299;&#64330;&#1514;
  }

  rightToLeftChars := 'ابتثجحخدذرزسشصضطظعغفقكلمنهويةאבּבגדהוזחטיכּכךלמםנןסעפּפףצץקרשׁשׂתּת';
  Result := (rightToLeftChars.IndexOfAny(text.ToCharArray()) <> -1);
end;
//---------------------------------------------------------------------------
function ExtractFieldContents(str: String; field: Integer; delimiter: String = ';'): String;
var
  i: Integer;
begin

  // Remove previous fields
  for i := 0 to field-1 do
  begin
    Delete(str, 1, (Pos(delimiter, str) - 1) + Length(delimiter));
  end;

  // Remove following fields
  if (Pos(delimiter, str) > 0) then
  begin
    i := Pos(delimiter, str);
    Delete(str, i, Length(str) - i + 1);
  end;

  Result := str;
end;
//---------------------------------------------------------------------------
procedure ReplaceTextRange(var sText: String; iStart: Integer; iEnd: Integer; c: Char);
var
  i: Integer;
begin
  for i := iStart to iEnd do
  begin
    sText[i] := c;
  end;
end;
//---------------------------------------------------------------------------
procedure Dump(sText: String; sDestFile: String);
var
  outputFile: TStringList;
begin
  outputFile := TStringList.Create();
  outputFile.Text := sText;
  outputFile.SaveToFile(sDestFile, TEncoding.UTF8);
  outputFile.Free;
end;
//---------------------------------------------------------------------------
// Make sure DoubleBuffered is enabled
procedure DrawGlassText(canvas: TCanvas; GlowSize: Integer; Text: UnicodeString; left, top, width, height: Integer; Format: DWORD; fontName: String; fontSize: Integer; fontStyles: TFontStyles = []; textColor: TColor = clWindowText); overload;
var
  textRect: TRect;
begin
  canvas.Font.Name := fontName;
  canvas.Font.Size := fontSize;
  canvas.Font.Style := fontStyles;
  textRect.Create(left, top, left + width, top + height);
  DrawGlassText(canvas.Handle, Text, textRect, Format, GlowSize, textColor);
end;
//---------------------------------------------------------------------------
// Delphi's glow effect on glass frames is very bad. This draw nicer texts and allows more glow levels.
procedure DrawGlassText(Canvas: TCanvas; GlowSize: Integer; var Rect: TRect; var Text: UnicodeString; Format: DWORD); overload;
var
  DTTOpts: TDTTOpts;
begin
  if Win32MajorVersion < 6 then
  begin
    DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Format);
    Exit;
  end;

  ZeroMemory(@DTTOpts, SizeOf(DTTOpts));
  DTTOpts.dwSize := SizeOf(DTTOpts);
  DTTOpts.dwFlags := DTT_COMPOSITED or DTT_TEXTCOLOR;

  if Format and DT_CALCRECT = DT_CALCRECT then
    DTTOpts.dwFlags := DTTOpts.dwFlags or DTT_CALCRECT;

  DTTOpts.crText := ColorToRGB(Canvas.Font.Color);

  if GlowSize > 0 then
  begin
    DTTOpts.dwFlags := DTTOpts.dwFlags or DTT_GLOWSIZE;
    DTTOpts.iGlowSize := GlowSize;
  end;

  with StyleServices.GetElementDetails(teEditTextNormal) do
    DrawThemeTextEx(StyleServices.Theme[teEdit], Canvas.Handle, Part, State,
      PWideChar(Text), Length(Text), Format, @Rect, DTTOpts);
end;
//---------------------------------------------------------------------------
procedure ILAddMasked(var ImgList: Vcl.Controls.TImageList; ResourceName: String; TransparentColor: TColor);
var
  bitmap: Vcl.Graphics.TBitmap;
begin
  bitmap := Vcl.Graphics.TBitmap.Create();
  try
    bitmap.LoadFromResourceName(hInstance, ResourceName);
    ImgList.AddMasked(bitmap, TransparentColor);
  finally
    bitmap.Free();
  end;
end;
//---------------------------------------------------------------------------
// Returns the text as how it will be drawed later (can be used for text width calculation).
// Text example: "foo(attr1,attr2 = CONST,attr3=)" (see beNotified() method)
function GetFormattedMethodString(str: String): String;
var
  parameters: TStringList;
  sParameterString: String;
  i: Integer;
begin
  Result := '';

  // Get parameters
  parameters := TStringList.Create();
  sParameterString := str;
  Delete(sParameterString, 1, Pos('(', sParameterString));
  sParameterString := Copy(sParameterString, 1, sParameterString.Length-1);
  while (Pos(',', sParameterString) > 0) do
  begin
    parameters.Add(Copy(sParameterString, 1, Pos(',', sParameterString)-1));
    Delete(sParameterString, 1, Pos(',', sParameterString));
  end;
  parameters.Add(sParameterString);  // Push last back

  Result := Copy(str, 1, Pos('(', str));  // Copy name + bracket

  // Append parameters
  for i := 0 to parameters.Count-1 do
  begin
    if (Pos('=', parameters.Strings[i]) > 0) then
    begin
      Result := Result + TrimRight(Copy(parameters.Strings[i], 1, Pos('=', sParameterString)-1));
    end
    else
    begin
      Result := Result + parameters.Strings[i];
    end;

    // Append param delimiter (except for last param)
    if (i < parameters.Count-1) then
    begin
      Result := Result + ', ';
    end;
  end;

  // Append last closing bracked
  Result := Result + ')';
end;
//---------------------------------------------------------------------------

end.
