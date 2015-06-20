//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit AnalyzerPHP;

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Generics.Collections,
  CRC, Vcl.Dialogs, Vcl.Forms, Windows, ParsingIndicatorForms, Vcl.ComCtrls,
  System.WideStrUtils;

type
  // Analyzer sets
  as_visibility = (asvUnknown, asvPublic, asvPrivate, asvProtected);
  as_direction = (asdNone, asdLeft, asdRight);

  crcdb = class
  public
    crc: UINT32;
    sFile: String;
  end;

  Ttextpos = class
  public
    iStart: UINT32;
    iEnd: UINT32;

    constructor Create(iStart: UINT32; iEnd: UINT32);
  end;

  AnalyzerPHPConstant = class
  public
    name: String;
    value: String;
    case_sensitive: Boolean;
  end;

  AnalyzerPHPAttribute = class
  public
    visibility: as_visibility;
    bStatic: Boolean;
    bConst: Boolean;
    name: String;
    fileOffset: UINT32;
  end;

  AnalyzerPHPMethod = class
  public
    visibility: as_visibility;
    bStatic: Boolean;
    name: String;
    parameters: TStringList;
    fileOffset: UINT32;

    constructor Create();
    destructor Destroy(); override;
  end;

  AnalyzerPHPClass = class
  public
    name: String;
    derivedFromClass: String;
    attributes: System.Generics.Collections.TObjectList<AnalyzerPHPAttribute>;
    methods: System.Generics.Collections.TObjectList<AnalyzerPHPMethod>;
    fileOffset: UINT32;

    constructor Create();
    destructor Destroy(); override;
    function countPublicAttributesAndMethods(checkType: Integer): Integer;  // checkType: 0 = both, 1 = attributes only, 2 = methods only
  end;

  ClassLinkingListEntry = class
  public
    classIndex: UINT32;
    fileIndices: System.Generics.Collections.TList<Integer>;  // Must be signed integer

    constructor Create();
    destructor Destroy(); override;
  end;

  TAnalyzerPHP = class
  public
    // Attributes
    isAnalyzing: Boolean;
    classes: System.Generics.Collections.TObjectList<AnalyzerPHPClass>;
    constants: System.Generics.Collections.TObjectList<AnalyzerPHPConstant>;
    include_files: TStringList;
    included_files: TStringList;
    RootDirectory: String;
    LastClass: String;
    iPos: Integer;
    iOldDirection: as_direction;
    bGetParamValues: Boolean;
    bGetParamTypeCasting: Boolean;
    text: ^String;

    // CRC file database
    dbfiles: System.Generics.Collections.TObjectList<crcdb>;

    // List for storing which classes are in which files (used for the Class Inspector)
    classLinkingList: System.Generics.Collections.TObjectList<ClassLinkingListEntry>;

    // Methods
    constructor Create();
    destructor Destroy(); override;
    procedure AnalyzeDirectory();
    function AnalyzePHP(currentFile: String = ''): boolean;
    procedure PrepareTextForParsing(bReplaceWithPlaceholders: Boolean = false);  // Try to write ReplaceTextRange() in Assembler
    procedure Inherit();
    function getNext(bFindKeywordOnly: Boolean = true): String;
    function getPrevious(bFindKeywordOnly: Boolean = true): String;
    procedure Reset(bResetAll: Boolean = false);
    function getLastClass(): AnalyzerPHPClass;
    procedure savePosition();
    procedure restorePosition();
    procedure dump(outputFile: String; mode: Integer);  // for debug testing

  private
    alterText: Boolean;  // Allow to set? Should always be set to true
    scannedFolders: Integer;
    scanningStart: DWORD;

    // Attributes for backup
    _iPos: Integer;
    _iOldDirection: as_direction;

    function isKeyword(sKeyword: String): boolean;
    function AllBracketsClosed(var str: String): boolean;
    function LastChar(var str: String; c: WideChar): Integer;
    procedure ListFiles(SearchDir: String; ScanOnly: Boolean);
    procedure LoadFile(sFile: String);
    function getClassByName(class_name: String): AnalyzerPHPClass;
    function getMethodByName(php_class: AnalyzerPHPClass; method_name: String): AnalyzerPHPMethod;
    function getAttributeByName(php_class: AnalyzerPHPClass; attribute_name: String): AnalyzerPHPAttribute;
    function getDBFilesIndexByFileName(filename: String): Integer;
  end;

const
  keywords: array[0..10] of string = ('class', 'private', 'public', 'protected', 'const', 'static', 'function', 'include', 'include_once', 'define', 'var');

implementation

uses
  cccplugin, ccchelperfunctions, System.StrUtils, ClassesListDockingForms;

//---------------------------------------------------------------------------
// Class: AnalyzerPHPMethod
constructor AnalyzerPHPMethod.Create();
begin
  self.parameters := TStringList.Create(true);
end;
//---------------------------------------------------------------------------
destructor AnalyzerPHPMethod.Destroy();
begin
  //FreeAndNil(self.parameters);
  self.parameters.Free();
  self.parameters := nil;
end;
//---------------------------------------------------------------------------
// Class: AnalyzerPHPClass
constructor AnalyzerPHPClass.Create();
begin
  self.attributes := System.Generics.Collections.TObjectList<AnalyzerPHPAttribute>.Create(true);
  self.methods := System.Generics.Collections.TObjectList<AnalyzerPHPMethod>.Create(true);
end;
//---------------------------------------------------------------------------
destructor AnalyzerPHPClass.Destroy();
begin
  self.attributes.Clear();
  self.methods.Clear();
  self.attributes.Free();
  self.methods.Free();
  self.attributes := nil;
  self.methods := nil;
end;
//---------------------------------------------------------------------------
function AnalyzerPHPClass.countPublicAttributesAndMethods(checkType: Integer): Integer;  // checkType: 0 = both, 1 = attributes only, 2 = methods only
var
  i: Integer;
begin
  Result := 0;

  if (checkType = 0) or (checkType = 1) then
  begin
    // Count public attributes
    for i := 0 to self.attributes.Count-1 do
    begin
      if (self.attributes.Items[i].visibility = asvPublic) then
        Inc(Result);
    end;
  end;

  if (checkType = 0) or (checkType = 2) then
  begin
    // Count public methods
    for i := 0 to self.methods.Count-1 do
    begin
      if (self.methods.Items[i].visibility = asvPublic) then
        Inc(Result);
    end;
  end;

end;
//---------------------------------------------------------------------------
// Class: ClassLinkingListEntry
constructor ClassLinkingListEntry.Create();
begin
  self.fileIndices := System.Generics.Collections.TList<Integer>.Create();
end;
//---------------------------------------------------------------------------
destructor ClassLinkingListEntry.Destroy();
begin
  self.fileIndices.Clear();
  self.fileIndices.Free();
  self.fileIndices := nil;
end;
//---------------------------------------------------------------------------
// Class: Ttextpos
constructor Ttextpos.Create(iStart: UINT32; iEnd: UINT32);
begin
  self.iStart := iStart;
  self.iEnd := iEnd;
end;
//---------------------------------------------------------------------------
// Class: TAnalyzerPHP
constructor TAnalyzerPHP.Create();
begin
  self.isAnalyzing := false;
  self.classes := System.Generics.Collections.TObjectList<AnalyzerPHPClass>.Create(true);
  self.constants := System.Generics.Collections.TObjectList<AnalyzerPHPConstant>.Create(true);
  self.include_files := TStringList.Create(true);
  self.included_files := TStringList.Create(true);
  self.dbfiles := System.Generics.Collections.TObjectList<crcdb>.Create(true);
  self.classLinkingList := System.Generics.Collections.TObjectList<ClassLinkingListEntry>.Create(true);

  self.iPos := 1;
  self.iOldDirection := asdNone;
  self.text := nil;
  self.RootDirectory := '';
  self.LastClass := '';
  self.alterText := true;
  self.bGetParamValues := false;

  self.savePosition();
end;
//---------------------------------------------------------------------------
destructor TAnalyzerPHP.Destroy();
begin
  // Delete all classes (this will invoke the destructor of each object (php class) which deletes their attributes & methods)
  self.classes.Clear();
  self.classes.Free();
  self.classes := nil;

  // Delete constants
  self.constants.Clear();
  self.constants.Free();
  self.constants := nil;

  // Delete stored CRC checksums from files in DB
  self.dbfiles.Clear();
  self.dbfiles.Free();
  self.dbfiles := nil;

  // Delete class linking list
  self.classLinkingList.Clear();
  self.classLinkingList.Free();
  self.classLinkingList := nil;

  // Delete other objects
  self.include_files.Free();
  self.include_files := nil;
  self.included_files.Free();
  self.included_files := nil;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.isKeyword(sKeyword: string): Boolean;
var
  i: UINT32;
  isKeyword: Boolean;
begin
  isKeyword := false;

  for i := 0 to Length(keywords)-1 do
  begin
    if
    (
      (Length(sKeyword) = Length(keywords[i])) and  // Is it faster first (& additionally) to check for length before comparing char by char for equality?
      (sKeyword = keywords[i])
    )
    then begin
      isKeyword := true;
      break;  // don't need here?
    end;
  end;

  Result := isKeyword;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getNext(bFindKeywordOnly: Boolean = true): String;
var
  returnNext: String;
  sWord: String;
  iStart: Integer;
  iEnd: Integer;
begin
  if not Assigned(self.text) then
  begin
    raise Exception.Create('AnalazerPHP: No text assigned');
  end;

  returnNext := '';

  if
  (
    (Length(self.text^) > 0) and
    (self.iPos > 0) and
    (self.iPos <= Length(self.text^) + 1)
  )
  then begin
    iStart := -1;

    for iEnd := self.iPos to Length(self.text^) do
    begin
      if
      (
        (self.text^[iEnd] = #32) or  // space
        (self.text^[iEnd] = #9) or   // tab
        (self.text^[iEnd] = #10) or  // line feed \n
        (self.text^[iEnd] = #13) or  // carriage return \r
        (iEnd = Length(self.text^))
      )
      then begin
        if
        (
          (iStart <> -1) and
          (iEnd > iStart)
        )
        then
        begin
          sWord := Copy(self.text^, iStart, iEnd - iStart);

          // Find keywords only?
          if (bFindKeywordOnly) then
          begin
            // Search for keywords only and found one
            if (self.isKeyword(LowerCase(sWord))) then
            begin
              // If we previously searched back, the internal pointer was positioned before the word.
              // If we now start searching from that position, we will find the last searched word again
              // (not if previously searched forth). To skip the last word, we need to skip one word.
              if
              (
                (self.iOldDirection = asdRight) or
                (self.iOldDirection = asdNone)
              )
              then begin
                self.iOldDirection := asdRight;
                break;
              end else begin
                self.iOldDirection := asdRight;
                iStart := -1;
                sWord := '';
              end;
            end
            // Search for keywords only but currect word is not a keyword => reset & search further
            else
            begin
              self.iOldDirection := asdRight;
              iStart := -1;
              sWord := '';
            end;
          end
          // Search for everything? Return found word
          else
          begin
            // If we previously searched back, the internal pointer was positioned before the word.
            // If we now start searching from that position, we will find the last searched word again
            // (not if previously searched forth). To skip the last word, we need to skip one word.
            if
            (
              (self.iOldDirection = asdRight) or
              (self.iOldDirection = asdNone)
            )
            then begin
              self.iOldDirection := asdRight;
              break;
            end else begin
              self.iOldDirection := asdRight;
              iStart := -1;
              sWord := '';
            end;
          end;
        end;
      end
      else
      begin
        if (iStart = -1) then
        begin
          iStart := iEnd;
        end;
      end;
    end;

    self.iPos := iif(iEnd > Length(self.text^), Length(self.text^), iEnd);
    returnNext := sWord;
  end;

  Result := returnNext;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getPrevious(bFindKeywordOnly: Boolean = true): String;
var
  returnNext: String;
  sWord: String;
  iStart: Integer;
  iEnd: Integer;
begin
  if not Assigned(self.text) then
  begin
    raise Exception.Create('AnalazerPHP: No text assigned');
  end;

  returnNext := '';

  if
  (
    (Length(self.text^) > 0) and
    (self.iPos > 0) and
    (self.iPos <= Length(self.text^) + 1)
  )
  then begin
    // We need to decrement by 1 char to get the previous char, not next
    if (self.iPos > 1) then begin
      self.iPos := self.iPos - 1;
    end;

    iStart := -1;

    for iEnd := self.iPos downto 1 do
    begin
      if
      (
        (self.text^[iEnd] = #32) or  // space
        (self.text^[iEnd] = #9) or   // tab
        (self.text^[iEnd] = #10) or  // line feed \n
        (self.text^[iEnd] = #13) or  // carriage return \r
        (iEnd = 1)
      )
      then begin
        if
        (
          (iStart <> -1) and
          (iEnd < iStart)
        )
        then
        begin
          sWord := Copy(self.text^, iEnd + Integer(iif(iEnd>1,1,0)), iStart - (iEnd + Integer(iif(iEnd>1,1,0))));

          // Find keywords only?
          if (bFindKeywordOnly) then
          begin
            // Search for keywords only and found one
            if (self.isKeyword(LowerCase(sWord))) then
            begin
              // If we previously searched forth, the internal pointer was positioned after the word.
              // If we now start searching from that position backwards, we will find the last searched word again
              // (not if previously searched back). To skip the last word, we need to skip one word.
              if
              (
                (self.iOldDirection = asdLeft) or
                (self.iOldDirection = asdNone)
              )
              then begin
                self.iOldDirection := asdLeft;
                break;
              end else begin
                self.iOldDirection := asdLeft;
                iStart := -1;
                sWord := '';
              end;
            end
            // Search for keywords only but currect word is not a keyword => reset & search further
            else
            begin
              self.iOldDirection := asdLeft;
              iStart := -1;
              sWord := '';
            end;
          end
          // Search for everything? Return found word
          else
          begin
            // If we previously searched back, the internal pointer was positioned before the word.
            // If we now start searching from that position, we will find the last searched word again
            // (not if previously searched forth). To skip the last word, we need to skip one word.
            if
            (
              (self.iOldDirection = asdLeft) or
              (self.iOldDirection = asdNone)
            )
            then begin
              self.iOldDirection := asdLeft;
              break;
            end else begin
              self.iOldDirection := asdLeft;
              iStart := -1;
              sWord := '';
            end;
          end;
        end;
      end
      else
      begin
        if (iStart = -1) then
        begin
          iStart := iEnd+1;
        end;
      end;
    end;

    self.iPos := iif(iEnd < 1, 1, iEnd);
    returnNext := sWord;
  end;

  Result := returnNext;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.savePosition();
begin
  self._iPos := self.iPos;
  self._iOldDirection := self.iOldDirection;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.restorePosition();
begin
  self.iPos := self._iPos;
  self.iOldDirection := self._iOldDirection;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.dump(outputFile: String; mode: Integer);
var
  sl: TStringList;
  iClass, i, j: Integer;
  line: String;
begin
  sl := TStringList.Create;

  // Dump classes, attributes, methods
  if (mode = 1) then
  begin
    for iClass := 0 to self.classes.Count-1 do
    begin
      // Add class name and, if available, the name of the class from which derived
      line := self.classes.Items[iClass].name;
      if Length(self.classes.Items[iClass].derivedFromClass) > 0 then
        line := line + ': ' + self.classes.Items[iClass].derivedFromClass;
      sl.Add(line);

      // Add attributes
      for j := 0 to self.classes.Items[iClass].attributes.Count-1 do
      begin
        case self.classes.Items[iClass].attributes.Items[j].visibility of
          asvPublic: line := '+';
          asvProtected: line := '#';
          asvPrivate: line := '-';
          else line := '?';  // no visibility is normally treated as public
        end;

        line := line + self.classes.Items[iClass].attributes.Items[j].name;

        if
        (
          (self.classes.Items[iClass].attributes.Items[j].bStatic) or
          (self.classes.Items[iClass].attributes.Items[j].bConst)
        )
        then begin
          line := line + ' [';

          if self.classes.Items[iClass].attributes.Items[j].bStatic then
            line := line + 'static,';

          if self.classes.Items[iClass].attributes.Items[j].bConst then
            line := line + 'const,';

          Delete(line, Length(line), 1);  // Remove last comma
          line := line + ']';
        end;

        sl.Add(line);
      end;

      // Add methods
      for j := 0 to self.classes.Items[iClass].methods.Count-1 do
      begin
        case self.classes.Items[iClass].methods.Items[j].visibility of
          asvPublic: line := '+';
          asvProtected: line := '#';
          asvPrivate: line := '-';
          else line := '?';  // no visibility is normally treated as public
        end;

        line := line + self.classes.Items[iClass].methods.Items[j].name;
        line := line + ' (' + StringReplace(self.classes.Items[iClass].methods.Items[j].parameters.Text, #13#10, ',', [rfReplaceAll]) + ')';

        line := line + ' [' + iif(self.classes.Items[iClass].methods.Items[j].bStatic, 'static', '') + ']';

        sl.Add(line);
      end;

      sl.Add('');
    end;
  end
  // Dump class linking list
  else if (mode = 2) then
  begin
    for i := 0 to self.classLinkingList.Count-1 do
    begin
      sl.Add(self.classes.Items[self.classLinkingList.Items[i].classIndex].name +' ['+ IntToStr(self.classLinkingList.Items[i].classIndex) +']:');

      for j := 0 to self.classLinkingList.Items[i].fileIndices.Count-1 do
      begin
        if (self.classLinkingList.Items[i].fileIndices.Items[j] <> -1) then
          sl.Add('  ' + self.dbfiles.Items[self.classLinkingList.Items[i].fileIndices.Items[j]].sFile + ' [' + IntToStr(self.classLinkingList.Items[i].fileIndices.Items[j]) +']');
      end;
    end;
  end;

  sl.SaveToFile(outputFile);
  sl.Free;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.AnalyzePHP(currentFile: String = ''): Boolean;
var
  inValidConstruct: Boolean;
  sKeyword: String;
  sCheckKeyword: String;
  s1, s2, s3, s4: String;
  b1, b2: Boolean;
  i1, i2, i3: Integer;
  php_class: AnalyzerPHPClass;
  php_attribute: AnalyzerPHPAttribute;
  php_method: AnalyzerPHPMethod;
  php_constant: AnalyzerPHPConstant;
begin

  // Remove comments, strings, non PHP data, etc.
  self.PrepareTextForParsing(true);

  // Parse
  // First try to go to PHP's valid starting tag
  b1 := false;
  repeat
    sKeyword := self.getNext(false);
    if (sKeyword = '<?php') then
    begin
      b1 := true;
    end;
  until (b1) or (Length(sKeyword) = 0);

  // Starting tag not found -> exit
  if (not b1) then
  begin
    Exit(false);
  end;

  inValidConstruct := false;

  // Note: Don't look for PHP's end tag. PHP/Zend even forbits it! (crazy...)
  while (true) do
  begin
    try
      sKeyword := self.getNext();
      sCheckKeyword := LowerCase(sKeyword);

      // Interfaces (currently not supported)
      if (sCheckKeyword = 'interface') then
      begin
        inValidConstruct := false;
      end

      // Class name (and parent of derived class)
      else if (sCheckKeyword = 'class') then
      begin

        // Get & create PHP class
        s1 := self.getNext(false);  // Class name

        // Fix problem with class names which contains the code block bracket ("{").
        // Perform a global replacement before getting the next word. This is somewhat slow,
        // but it should not happen often. Example: "class foo{" => "class foo {".
        if s1[Length(s1)] = '{' then
        begin
          Delete(s1, Length(s1), 1);
          self.text^ := StringReplace(self.text^, s1 + '{', s1 + ' {', []);  // No case-sensitivity and no global replace. Replace only once. The outer loop will not become infinite with this anymore.
        end;

        s2 := LowerCase(self.getNext(false));  // bracket or extends
        if (s2 = 'implements') then
        begin
          inValidConstruct := false;
        end
        else if (s2 = 'extends') then
        begin

          inValidConstruct := true;
          self.LastClass := s1;

          // Check if already added
          b1 := false;
          for i1 := 0 to self.classes.Count-1 do
          begin
            if (LowerCase(self.classes.Items[i1].name) = LowerCase(s1)) then
            begin
              b1 := true;
              break;
            end;
          end;

          // Not found
          if (not b1) then
          begin
            i2 := self.classes.Add(AnalyzerPHPClass.Create());
            php_class := self.classes.Last;
            php_class.name := s1;
            php_class.derivedFromClass := self.getNext(false);  // name of base class

            // Create class linking entry (if old file entry not found, getDBFilesIndexByFileName() returns -1 which means the class was defined in current scintilla view)
            self.classLinkingList.Add(ClassLinkingListEntry.Create());
            self.classLinkingList.Last.classIndex := i2;
            i2 := self.getDBFilesIndexByFileName(currentFile);
            if (not self.classLinkingList.Last.fileIndices.Contains(i2)) then
              self.classLinkingList.Last.fileIndices.Add(i2);
          end
          // Found
          else
          begin
            // If old class found, kill attributes and methods (because we don't know which of them are still used)
            php_class := self.classes.Items[i1];
            php_class.name := s1;  // Reassign. Actually unneccessary but maybe the case-sensitivity changed..
            php_class.derivedFromClass := self.getNext(false);

            // Delete all attributes
            php_class.attributes.Clear();

            // Delete all methods
            php_class.methods.Clear();

            // Update class linking entry (if old file entry not found, getDBFilesIndexByFileName() returns -1 which means the class was defined in current scintilla view)
            i2 := self.getDBFilesIndexByFileName(currentFile);
            if (not self.classLinkingList.Items[i1].fileIndices.Contains(i2))
            then
              self.classLinkingList.Items[i1].fileIndices.Add(i2);
          end;
        end
        else if (s2 = '{') then
        begin
          inValidConstruct := true;

          // Even if already added, save last found class name
          self.LastClass := s1;

          // Check if already added
          b1 := false;
          for i1 := 0 to self.classes.Count-1 do
          begin
            if (LowerCase(self.classes.Items[i1].name) = LowerCase(s1)) then
            begin
              b1 := true;
              break;
            end;
          end;

          // Not found
          if (not b1) then
          begin
            i2 := self.classes.Add(AnalyzerPHPClass.Create());
            php_class := self.classes.Last;
            php_class.name := s1;

            // Create class linking entry (if old file entry not found, getDBFilesIndexByFileName() returns -1 which means the class was defined in current scintilla view)
            self.classLinkingList.Add(ClassLinkingListEntry.Create());
            self.classLinkingList.Last.classIndex := i2;
            i2 := self.getDBFilesIndexByFileName(currentFile);
            if (not self.classLinkingList.Last.fileIndices.Contains(i2)) then
              self.classLinkingList.Last.fileIndices.Add(i2);
          end
          // Found
          else
          begin
            // If old class found, kill attributes and methods (because we don't know which of them are still used)
            php_class := self.classes.Items[i1];
            php_class.name := s1;  // Reassign. Actually unneccessary but maybe the case-sensitivity changed..
            php_class.derivedFromClass := '';

            // Delete all attributes
            php_class.attributes.Clear();

            // Delete all methods
            php_class.methods.Clear();

            // Update class linking entry (if old file entry not found, getDBFilesIndexByFileName() returns -1 which means the class was defined in current scintilla view)
            i2 := self.getDBFilesIndexByFileName(currentFile);
            if (not self.classLinkingList.Items[i1].fileIndices.Contains(i2))
            then
              self.classLinkingList.Items[i1].fileIndices.Add(i2);
          end;
        end;
      end

      // Note: Since PHP is a runtime interpreted language, we can't (and don't need to) collect additional files from include/require statements. Also, we don't need constant definitions.
(*
      else if (sCheckKeyword == "include" || sCheckKeyword == "include_once")
      {
        // s1 may contain only the bracket (exmaple: 'include ( "xx")'). Then find next
        do
        {
          s1 = this->getNext(false);
        }
        while (Length(s1) > 0 && s1.Pos("\"") == 0 && s1.Pos("'") == 0);

        s2 = (s1.Pos("\"") > 0) ? "\"" : "'";
        s1 = s1.Delete(1, s1.Pos(s2));
        s1 = s1.SubString(1, s1.Pos(s2)-1);
        if (Length(s1) > 0)
        {
          this->include_files.push_back(s1);
        }
      }
      else if (sCheckKeyword == "define")
      {
        // Get all parameters with brackets
        // s1 = all parameters
        // s2 = next word
        s1 = "";
        do
        {
          s2 = this->getNext(false);
          s1 = s1 + s2;
        }
        while (Length(s2) > 0 && !this->AllBracketsClosed(&s1));

        // Remove first & last brackets
        s1.Delete(1, s1.Pos("("));
        s1 = s1.SubString(1, this->LastChar(&s1, ')')-1);

        // Extract multiple parameters
        if (s1.Pos(",") > 0)
        {
          s4 = "";  // constant name
          s5 = "";  // constant value
          s6 = "";  // handle constant name case-sensitive?

          //## Get first parameter (constant name)
            s2 = s1.SubString(1, s1.Pos(",")-1).Trim();
            s1.Delete(1, s1.Pos(","));

            // Remove quotes
            s3 = (s2.Pos("\"") > 0) ? "\"" : "'";
            s2.Delete(1, s2.Pos(s3));
            s4 = s2.SubString(1, this->LastChar(&s2, s3[1])-1).Trim();

          //## Get second parameter (constant value)
          // Note 1: Value can be string, number, decimal, object, other constant,...
          //         so just trim first and look if enclosed in brackets, otherwise just take the entire value as-is
          // Note 2: There is a third optional parameter for case-sensitivity
            if (s1.Pos(",") > 0)
            {
              s2 = s1.SubString(1, s1.Pos(",")-1).Trim();
              s1.Delete(1, s1.Pos(","));
            }
            else
            {
              s2 = s1.Trim();
              s1 = "";  // set empty for pass third optional parameter
            }

            if
            (
              (Length(s2) > 1) &&  // atleast 2 chars (quotes)
              (
                (s2[1] == '"') &&
                (*s2.LastChar() == '"')
              ) ||
              (
                (s2[1] == '\'') &&
                (*s2.LastChar() == '\'')
              )
            )
            {
              // Remove first and last quotes
              s2 = s2.SubString(2, Length(s2)-2);
            }

            // Note: Don't trim value! It might be a string, where leading & following spaces are intended.
            s5 = s2;

          //## Optionally: Get third parameter (case-sensivity)
          s6 = s1.LowerCase().Trim();
          if
          (
            (Length(s6) > 0) &&
            (
              (s6 == "true") ||
              (s6 == "1")
            )
          )
          {
            s6 = "1";
          }
          else
          {
            s6 = "0";
          }

          //## Now create constant

          // According to PHP documentation, case-insensitive constant names are lower-cased.
          if (s6 == "1")
          {
            s4 = s4.LowerCase();
          }

          // Check if constant already got (then overwrite)
          b1 = false;
          for (i1=0; i1 < this->constants.size(); i1++)
          {
            if (this->constants[i1]->name == s4)
            {
              b1 = true;
              php_constant = this->constants[i1];
              break;
            }
          }

          // Get or create & get constant
          if (!b1)
          {
            this->constants.push_back(new AnalyzerPHPConstant());
            php_constant = this->constants.back();
          }

          // Save all values
          php_constant->name = s4;
          php_constant->value = s5;
          php_constant->case_sensitive = (s6 == "1");
        }
      }
*)

      else if (sCheckKeyword = 'const') then
      begin
        if inValidConstruct then
        begin
          // TODO: Constants declared & defined with "const" prefix are supported to be inside classes only. But they are allowed outside them too (since PHP 5.3.0)...
          if (self.LastClass <> '') then  // This works only if max. one class in file & non-class constants not defined between two classes
          begin
            s1 := self.getNext(false);

            // Get word starting position
            i3 := self.iPos - Length(s1);

            // Get attribute name only
            if (Length(s1) > 0) then
            begin
              if (s1[1] = '$') then
              begin
                Delete(s1, 1, 1);
              end;

              if (Pos('=', s1) > 0) then
              begin
                s1 := Copy(s1, 1, Pos('=', s1)-1);
              end
              else if (Pos(';', s1) > 0) then
              begin
                s1 := Copy(s1, 1, Pos(';', s1)-1);
              end;
            end;

            php_class := self.getLastClass();
            php_class.attributes.Add(AnalyzerPHPAttribute.Create());
            php_attribute := php_class.attributes.Last;
            php_attribute.visibility := asvPublic;  // constants in classes are always public (?), old: asvUnknown;
            php_attribute.bStatic := false;
            php_attribute.bConst := true;
            php_attribute.name := s1;
            php_attribute.fileOffset := i3;
          end;
        end;
      end
      else if
      (
        (sCheckKeyword = 'var') or
        (sCheckKeyword = 'public') or
        (sCheckKeyword = 'private') or
        (sCheckKeyword = 'protected')
      )
      then begin
        if inValidConstruct then
        begin
          b1 := false;  // static?
          b2 := false;  // function?
          s1 := '';  // name
          s2 := '';  // attributes (optionally)

          // Get visibility & name of method or attribute (+ if it is static)
          s2 := self.getNext(false);
          if (LowerCase(s2) = 'function') then
          begin
            b1 := false;
            b2 := true;
            s1 := self.getNext(false);
          end
          else if (LowerCase(s2) = 'static') then
          begin
            b1 := true;
            s3 := self.getNext(false);
            if (LowerCase(s3) = 'function') then
            begin
              b2 := true;
              s1 := self.getNext(false);
            end
            else
            begin
              b2 := false;
              s1 := s3;
            end;
          end
          else
          begin
            b1 := false;
            b2 := false;
            s1 := s2;
          end;

          // Get last (current) class
          php_class := self.getLastClass();

          // Create method / attribute
          if (b2) then  // function
          begin
            // Get word starting position
            i3 := self.iPos - Length(s1);

            php_class.methods.Add(AnalyzerPHPMethod.Create());
            php_method := php_class.methods.Last;
            if (sCheckKeyword = 'public') then begin
              php_method.visibility := asvPublic; end
            else if (sCheckKeyword = 'private') then begin
              php_method.visibility := asvPrivate; end
            else if (sCheckKeyword = 'protected') then begin
              php_method.visibility := asvProtected; end;
            php_method.bStatic := b1;
            php_method.name := s1;
            php_method.fileOffset := i3;

            //## Get parameters for function

            // Opening bracket in function name (because no space, tab, ect. between)
            if (Pos('(', s1) > 0) then
            begin
              //php_method.name := Trim(Copy(php_method.name, 1, Pos('(', php_method.name)-1));  // Trim() is thereotically not neccessary because name wouldn't contain bracket
              php_method.name := Copy(php_method.name, 1, Pos('(', php_method.name)-1);  // Trim() is thereotically not neccessary because name wouldn't contain bracket
              Delete(s1, 1, Pos('(', s1)-1);
            end
            // Bracket not in function name -> Find next opening bracket
            else
            begin
              repeat
                s1 := self.getNext(false);
              until (Length(s1) = 0) or (Pos('(', s1) > 0);
            end;

            // Get parameters until all brackets found
            s2 := 'x';  // next word
            while (not self.AllBracketsClosed(s1)) and (Length(s2) > 0) do
            begin
              s2 := self.getNext(false);
              s1 := s1 + ' ' + s2;  // Add empty space to separate possible casting name from parameter name
            end;

            // Remove first & last brackets
            Delete(s1, 1, Pos('(', s1));
            s1 := Copy(s1, 1, self.LastChar(s1, ')')-1);

            // Extract multiple parameters
            while (Pos(',', s1) > 0) do
            begin
              s2 := Trim(Copy(s1, 1, Pos(',', s1)-1));
              Delete(s1, 1, Pos(',', s1));
              if (Length(s2) > 0) then
              begin
                // Remove type casting
                if (not bGetParamTypeCasting) then
                begin
                  // Note: Parameters always start with $ or & (empty spaces, line breaks and other invalid chars are removed already).
                  // So just check for first char to check for casting - this is much faster than Copy()!
                  // Note 2: Actually, it is even faster to check if first char is not $ or &.
                  s4 := LowerCase(s2[1]);

                  if
                  (
                    not
                    (
                      (s4 = '$') or
                      (s4 = '&')
                    )
  {
                    (s4 = 'i') or  // int / integer
                    (s4 = 'b') or  // bool / boolean / binary
                    (s4 = 'f') or  // float
                    (s4 = 'd') or  // double
                    (s4 = 'r') or  // real
                    (s4 = 's') or  // string
                    (s4 = 'a') or  // array
                    (s4 = 'o') or  // object
                    (s4 = 'u')     // unset
  }
                  )
                  then begin
                    Delete(s2, 1, Pos(' ', s2));
                  end;
                end
                else
                begin
                  s2 := StringReplace(s2, '$', '', []);  // Replace only once to avoid wasting performance
                end;

                // Remove $ and optionally =
                if (s2[1] = '$') then
                begin
                  Delete(s2, 1, 1);
                end
                //else if (Length(s2) > 1) and (s2[1] = '&') and (s2[2] = '$') then  // Remove $ but keep address operator
                else if (Length(s2) > 1) and (s2[1] = '&') then  // Remove $ but keep address operator <- use this because a bit faster
                begin
                  Delete(s2, 2, 1);
                end;

                if (not self.bGetParamValues) and (Pos('=', s2) > 0) then
                begin
                  s2 := Copy(s2, 1, Pos('=', s2)-1);
                end;

                if (Length(s2) > 0) then
                begin
                  php_method.parameters.Add(s2);
                end;
              end;
            end;

            // Extract none or last parameter
            s1 := Trim(s1);
            if (Length(s1) > 0) then
            begin
              // Remove type casting
              if (not bGetParamTypeCasting) then
              begin
                // Note: Parameters always start with $ or & (empty spaces, line breaks and other invalid chars are removed already).
                // So just check for first char to check for casting - this is much faster than Copy()!
                // Note 2: Actually, it is even faster to check if first char is not $ or &.
                s4 := LowerCase(s1[1]);

                if
                (
                  not
                  (
                    (s4 = '$') or
                    (s4 = '&')
                  )
  {
                  (s4 = 'i') or  // int / integer
                  (s4 = 'b') or  // bool / boolean / binary
                  (s4 = 'f') or  // float
                  (s4 = 'd') or  // double
                  (s4 = 'r') or  // real
                  (s4 = 's') or  // string
                  (s4 = 'a') or  // array
                  (s4 = 'o') or  // object
                  (s4 = 'u')     // unset
  }
                )
                then begin
                  Delete(s1, 1, Pos(' ', s1));
                end;
              end
              else
              begin
                s2 := StringReplace(s2, '$', '', []);  // Replace only once to avoid wasting performance
              end;

              // Remove $ and optionally =
              if (s1[1] = '$') then
              begin
                Delete(s1, 1, 1);
              end
              //else if (Length(s1) > 1) and (s1[1] = '&') and (s1[2] = '$') then  // Remove $ but keep address operator
              else if (Length(s1) > 1) and (s1[1] = '&') then  // Remove $ but keep address operator <- use this because a bit faster
              begin
                Delete(s1, 2, 1);
              end;

              if (not self.bGetParamValues) and (Pos('=', s1) > 0) then
              begin
                s1 := Copy(s1, 1, Pos('=', s1)-1);
              end;

              if (Length(s1) > 0) then
              begin
                php_method.parameters.Add(s1);
              end;
            end;
          end
          else  // attribute
          begin
            // Get word starting position
            i3 := self.iPos - Length(s1);

            // Get attribute name only
            if (Length(s1) > 0) then
            begin
              if (s1[1] = '$') then
              begin
                Delete(s1, 1, 1);
              end;

              if (Pos('=', s1) > 0) then
              begin
                s1 := Copy(s1, 1, Pos('=', s1)-1);
              end
              else if (Pos(';', s1) > 0) then
              begin
                s1 := Copy(s1, 1, Pos(';', s1)-1);
              end;
            end;

            php_class.attributes.Add(AnalyzerPHPAttribute.Create());
            php_attribute := php_class.attributes.Last;
            if
            (
              (sCheckKeyword = 'var') or
              (sCheckKeyword = 'public')
            )
            then begin
              php_attribute.visibility := asvPublic; end
            else if (sCheckKeyword = 'private') then begin
              php_attribute.visibility := asvPrivate; end
            else if (sCheckKeyword = 'protected') then begin
              php_attribute.visibility := asvProtected; end;
            php_attribute.bConst := false;
            php_attribute.bStatic := b1;
            php_attribute.name := s1;
            php_attribute.fileOffset := i3;
          end;
        end;
      end;

      // Leave loop if no more keywords found
      if (Length(sKeyword) = 0) then
      begin
        break;
      end;
    except
      on E : Exception do
      begin
        // If there are parsing errors, just leave & return false.
        MessageBox(0, PWideChar(e.Message + sLineBreak + sLineBreak + 'File: ' + currentFile), 'Parsing error', 16);
        Exit(false);
      end;
    end;
  end;

  Result := true;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.AllBracketsClosed(var str: string): Boolean;
var
  i: Integer;
  open_brackets: Integer;
begin
  open_brackets := 0;

  for i := 1 to Length(str) do
  begin
    if (str[i] = '(')
    then begin
      Inc(open_brackets);
    end
    else if (str[i] = ')')
    then begin
      Dec(open_brackets);
    end;
  end;

  Result := (open_brackets <= 0);
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.LastChar(var str: String; c: WideChar): Integer;
var
  return: Integer;
  i: Integer;
begin
  return := 0;

  for i := Length(str) downto 1 do
  begin
    if (str[i] = c) then begin
      return := i;
    end;
  end;

  Result := return;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.AnalyzeDirectory();
begin

  if (Length(self.RootDirectory) = 0) then begin
    raise Exception.Create('Root directory not set');
  end;

  self.RootDirectory := IncludeTrailingPathDelimiter(self.RootDirectory);

  if not DirectoryExists(self.RootDirectory) then begin
    raise Exception.Create('Root directory not found');
  end;

  // Create parsing status window & start timer
  if (not Assigned(ParsingIndicatorForm)) then
  begin
    ParsingIndicatorForm := TParsingIndicatorForm.Create(Npp);
  end;

  ParsingIndicatorForm.ProgressBar1.Max := 100;
  ParsingIndicatorForm.ProgressBar1.Position := 100;
  ParsingIndicatorForm.ProgressBar1.State := TProgressBarState.pbsPaused;
  self.scanningStart := GetTickCount();

  // Scan folder structure first & update progress bar
  self.scannedFolders := 0;
  self.ListFiles(self.RootDirectory, true);
  ParsingIndicatorForm.ProgressBar1.Max := self.scannedFolders;
  ParsingIndicatorForm.ProgressBar1.Position := 0;
  ParsingIndicatorForm.ProgressBar1.State := TProgressBarState.pbsNormal;

  // Parse files recursively
  self.ListFiles(self.RootDirectory, false);

  // Make inheritage
  self.Inherit();

  // Close status window if opened
  ParsingIndicatorForm.Close();

end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.LoadFile(sFile: string);
var
  inputFile: TStringList;
  tmp: String;
  i: Integer;
  CRC: TCRCDef;
  fileChecksum: Cardinal;
  bParse: Boolean;
  bFound: Boolean;
begin
  // Reset (some) values for next file
  self.Reset(false);

  try
    inputFile := TStringList.Create(true);
    inputFile.LoadFromFile(sFile, TEncoding.ANSI);
    if (IsUTF8String(inputFile.Text)) then
      inputFile.LoadFromFile(sFile, TEncoding.UTF8);
    tmp := inputFile.Text;
    FreeAndNil(inputFile);

    // Compute CRC of file
    CRCInit(CRC, CRC_32);
    CRCCode(CRC, PChar(tmp)^, Length(tmp) * SizeOf(Char));
    fileChecksum := CRCDone(CRC);

    // Before parsing, check if already parsed & unchanged
    bParse := true;
    bFound := false;

    for i := 0 to self.dbfiles.Count-1 do
    begin
      if (self.dbfiles.Items[i].sFile = sFile) then
      begin
        // File found & CRC didn't changed => don't parse
        if (self.dbfiles.Items[i].crc = fileChecksum) then
        begin
          bParse := false;
        end
        // File found but CRC changed => save new CRC & parse
        else
        begin
          self.dbfiles.Items[i].crc := fileChecksum;
        end;

        bFound := true;
        break;
      end;
    end;

    if (bParse) then
    begin
      // Add loaded file
      if not bFound then
      begin
        self.dbfiles.Add(crcdb.Create());
        self.dbfiles.Last.crc := fileChecksum;
        self.dbfiles.Last.sFile := sFile;
      end;

      // Set text and parse
      self.text := @tmp;

      self.AnalyzePHP(sFile);
    end;
  except
    on E : Exception do
      Application.MessageBox(PWideChar(e.Message + sLineBreak + sLineBreak +'File: '+ sFile), 'Error in LoadFile()', 0+16);
  end;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.ListFiles(SearchDir: String; ScanOnly: Boolean);
var
  SearchResult: TSearchRec;
  itm, oldDir, fileExt: String;
begin

  if (SearchDir <> '') then
  begin
    oldDir := IncludeTrailingPathDelimiter(SearchDir);
    if (Copy(SearchDir, Length(SearchDir) - 3, 4) <> '\*.*') then
    begin
      SearchDir := SearchDir + '\*.*';
    end;

    try
      // Find at least one file?
      if (System.SysUtils.FindFirst(SearchDir, faAnyFile, SearchResult) = 0) then
      begin
        itm := SearchResult.Name;

        // First file
        if
        (
          (itm <> '.') and
          (itm <> '..')
        )
        then begin
          if (FileExists(oldDir + itm)) then  // File
          begin
            fileExt := ExtractFileExt(LowerCase(itm));
            if
            (
              (fileExt = '.php') or
              (fileExt = '.php6') or
              (fileExt = '.php5') or
              (fileExt = '.php4')
            )
            then begin
              if (not ScanOnly) then
                self.LoadFile(oldDir + itm);
            end;
          end
          else if (DirectoryExists(oldDir + itm)) then  // Folder
          begin
            // If parsing status form not visible but parsing runs too long, show parsing status form
            if (GetTickCount() - self.scanningStart > 5000) then
            begin
              if (not ParsingIndicatorForm.Visible) then
                ParsingIndicatorForm.Show()
              else
                Application.ProcessMessages();
            end;

            if (ScanOnly) then
              Inc(self.scannedFolders)
            else
              ParsingIndicatorForm.ProgressBar1.Position := ParsingIndicatorForm.ProgressBar1.Position + 1;

            self.ListFiles(oldDir + itm, ScanOnly);
          end;
        end;

        // Find next file
        while (System.SysUtils.FindNext(SearchResult) = 0) do
        begin
          itm := SearchResult.Name;
          if
          (
            (itm <> '.') and
            (itm <> '..')
          )
          then begin
            if (FileExists(oldDir + itm)) then  // Datei
            begin
              fileExt := ExtractFileExt(LowerCase(itm));
              if
              (
                (fileExt = '.php') or
                (fileExt = '.php6') or
                (fileExt = '.php5') or
                (fileExt = '.php4')
              )
              then begin
                if (not ScanOnly) then
                  self.LoadFile(oldDir + itm);
              end;
            end
            else if (DirectoryExists(oldDir + itm)) then  // Ordner
            begin

              // If parsing status form not visible but parsing runs too long, show parsing status form
              if (GetTickCount() - self.scanningStart > 5000) then
              begin
                if (not ParsingIndicatorForm.Visible) then
                  ParsingIndicatorForm.Show()
                else
                  Application.ProcessMessages();
              end;

              if (ScanOnly) then
                Inc(self.scannedFolders)
              else
                ParsingIndicatorForm.ProgressBar1.Position := ParsingIndicatorForm.ProgressBar1.Position + 1;

              self.ListFiles(oldDir + itm, ScanOnly);
            end;
          end;
        end;

        // Free memory
        System.SysUtils.FindClose(SearchResult);
      end;
    except
    end;
  end;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.Reset(bResetAll: Boolean = False);
begin
  self.iPos := 1;
  self.iOldDirection := asdNone;
  self._iPos := self.iPos;
  self._iOldDirection := self.iOldDirection;
  self.text := nil;
  self.LastClass := '';

  if (bResetAll) then
  begin
    self.dbfiles.Clear();
    self.RootDirectory := '';
    self.alterText := true;
    self.include_files.Clear();
    self.included_files.Clear();
    self.bGetParamValues := false;

    // Clear class linking list
    self.classLinkingList.Clear();

    // Delete all classes (this will invoke the destructor of each object (php class) which deletes their attributes & methods)
    self.classes.Clear();

    // Delete constants
    self.constants.Clear();
  end;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getLastClass(): AnalyzerPHPClass;
var
  return: AnalyzerPHPClass;
  iClass: Integer;
begin
  return := nil;

  if (self.LastClass <> '') then
  begin
    for iClass := 0 to self.classes.Count-1 do
    begin
      if (LowerCase(self.classes.Items[iClass].name) = LowerCase(self.LastClass)) then
      begin
        return := self.classes.Items[iClass];
      end;
    end;
  end;

  Result := return;
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.PrepareTextForParsing(bReplaceWithPlaceholders: Boolean = false);
var
  cLineDelimiter: Char;
  comments: System.Generics.Collections.TObjectList<Ttextpos>;
  cLastQuote: Char;
  i, j, iStart, iEnd: UINT32;
  bInString: Boolean;
  bInPHPCode: Boolean;
  commentType: Integer;  // 0 = none, 1 == //, 2 == /**/
  textnocomments: String;
  textnostrings: String;
  textnononphpcode: String;
  text_length: UINT32;
  iComment: Integer;
  iComments: Integer;
  tp_strings: System.Generics.Collections.TObjectList<Ttextpos>;
begin

  //****************************************************************
  //**                      Remove comments                       **
  //****************************************************************
  comments := System.Generics.Collections.TObjectList<Ttextpos>.Create(true);

  // Find out, what value is used for line breaks
  cLineDelimiter := #10;  // Line feed \n
  if (Pos(#13#10, self.text^) > 0) then
  begin
    cLineDelimiter := #13;
  end
  else if (Pos(#13, self.text^) > 0) then
  begin
    cLineDelimiter := #13;
  end
  else if (Pos(#10, self.text^) > 0) then
  begin
    cLineDelimiter := #10;
  end;

  cLastQuote := #0;
  bInString := false;
  commentType := 0;  // 0 = none, 1 == //, 2 == /**/
  iStart := 0;

  j := Length(self.text^);
  for i := 1 to j do
  begin
    // Switch between in-string / beyond-string (but only if not inside a comment)
    if
    (
      (self.text^[i] = '"') and
      (commentType = 0)
    )
    then begin
      if (cLastQuote = '"') then
      begin
        bInString := false;
        cLastQuote := #0;
      end
      else if (cLastQuote = #0) then
      begin
        bInString := true;
        cLastQuote := '"';
      end;
    end
    else if
    (
      (self.text^[i] = '''') and  // single quote (thanks delphi....)
      (commentType = 0)
    )
    then begin
      if (cLastQuote = '''') then  // single quote (thanks delphi....)
      begin
        bInString := false;
        cLastQuote := #0;
      end
      else if (cLastQuote = #0) then
      begin
        bInString := true;
        cLastQuote := '''';
      end;
    end
    else
    begin
      if (not bInString) then
      begin
        if
        (
          (commentType = 0) and
          (i < j) and
          (self.text^[i] = '/') and
          (self.text^[i+1] = '/')
        )
        then begin
          commentType := 1;
          iStart := i;
        end
        else if
        (
          (commentType = 0) and
          (i < j) and
          (self.text^[i] = '/') and
          (self.text^[i+1] = '*')
        )
        then begin
          commentType := 2;
          iStart := i;
        end
        else if
        (
          (commentType = 1) and
          (self.text^[i] = cLineDelimiter)  // end of line
        )
        then begin
          iEnd := i;  // Don't delete line break delimiter
          commentType := 0;
          comments.Add(Ttextpos.Create(iStart, iEnd));
        end
        else if
        (
          (commentType = 2) and
          (i < j) and
          (self.text^[i] = '*') and
          (self.text^[i+1] = '/')
        )
        then begin
          iEnd := i+2;  // Delete comment end (*/)
          commentType := 0;
          comments.Add(Ttextpos.Create(iStart, iEnd));
        end;
      end;
    end;
  end;

  // All comments (their positions) found - now copy non-comment segments
  // Change text only if comments found
  if (comments.Count > 0) then
  begin
    if (bReplaceWithPlaceholders) then
    begin
      iComments := comments.Count;
      for iComment := 0 to iComments-1 do
      begin
        ReplaceTextRange(self.text^, comments.Items[iComment].iStart, comments.Items[iComment].iEnd-1, ' ');
      end;
    end
    else
    begin
      // If only one comment, use Delete()
      if (comments.Count = 1) then
      begin
        Delete(self.text^, comments.Items[0].iStart, comments.Items[0].iEnd - comments.Items[0].iStart);
      end
      // For multiple comments use Copy()
      else
      begin
        textnocomments := '';
        text_length := Length(self.text^);

        iComments := comments.Count;
        for iComment := 0 to iComments-1 do
        begin
          // Copy from first position to beginning of current comment
          if (iComment = 0) then
          begin
            textnocomments := textnocomments + Copy(self.text^, 1, comments.Items[iComment].iStart - 1);
          end
          else
          begin
            // Copy from end of previous comment to beginningof current comment
            textnocomments := textnocomments + Copy(self.text^, comments.Items[iComment-1].iEnd, comments.Items[iComment].iStart - comments.Items[iComment-1].iEnd);

            // Additionally: If last element, also copy from end of current comment to end of entire source string
            if
            (
              (iComment = iComments-1) and
              (comments.Items[iComment].iEnd < text_length-2)
            )
            then begin
              textnocomments := textnocomments + Copy(self.text^, comments.Items[iComment].iEnd, (text_length - comments.Items[iComment].iEnd) + 1);
            end;
          end;
        end;

        // This is more efficient than copying
        self.text^ := textnocomments;  // Note: In C++Builder there is a swap() function which is much faster. When it will be available in Delphi too, use it then.
      end;
    end;
  end;

  // Free (TObjectList will delete objects automatically - no need to delte or clear)
  comments.Free();

  //****************************************************************
  //**                       Remove strings                       **
  //****************************************************************
  // Note: "define", "include" and "include_once" will not work with this and so has been commented out
  tp_strings := System.Generics.Collections.TObjectList<Ttextpos>.Create(true);
  cLastQuote := #0;
  //bInString := false;

  for i := 1 to Length(self.text^) do
  begin
    // Switch between in-string / beyond-string (but only if not inside a comment)
    if (self.text^[i] = '"') then
    begin
      if (cLastQuote = '"') then
      begin
        //bInString := false;
        cLastQuote := #0;
        iEnd := i+1;
        tp_strings.Add(Ttextpos.Create(iStart, iEnd));
      end
      else if (cLastQuote = #0) then
      begin
        //bInString := true;
        cLastQuote := '"';
        iStart := i;
      end;
    end
    else if (self.text^[i] = '''') then
    begin
      if (cLastQuote = '''') then
      begin
        //bInString := false;
        cLastQuote := #0;
        iEnd := i+1;
        tp_strings.Add(Ttextpos.Create(iStart, iEnd));
      end
      else if (cLastQuote = #0) then
      begin
        //bInString := true;
        cLastQuote := '''';
        iStart := i;
      end;
    end;
  end;

  // All strings (their positions) found - now copy non-string segments
  // Change text only if strings found
  if (tp_strings.Count > 0) then
  begin
    if (bReplaceWithPlaceholders) then
    begin
      iComments := tp_strings.Count;
      for iComment := 0 to iComments-1 do
      begin
        ReplaceTextRange(self.text^, tp_strings.Items[iComment].iStart, tp_strings.Items[iComment].iEnd-1, ' ');
      end;
    end
    else
    begin
      // If only one comment, use Delete()
      if (tp_strings.Count = 1) then
      begin
        Delete(self.text^, tp_strings.Items[0].iStart, tp_strings.Items[0].iEnd - tp_strings.Items[0].iStart);
      end
      // For multiple strings use Copy()
      else
      begin
        textnostrings := '';
        text_length := Length(self.text^);

        iComments := tp_strings.Count;
        for iComment := 0 to iComments-1 do
        begin
          // Copy from first position to beginning of current comment
          if (iComment = 0) then
          begin
            textnostrings := textnostrings + Copy(self.text^, 1, tp_strings.Items[iComment].iStart-1);
          end
          else
          begin
            // Copy from end of previous comment to beginningof current comment
            textnostrings := textnostrings + Copy(self.text^, tp_strings.Items[iComment-1].iEnd, tp_strings.Items[iComment].iStart - tp_strings.Items[iComment-1].iEnd);

            // Additionally: If last element, also copy from end of current comment to end of entire source string
            if
            (
              (iComment = iComments-1) and
              (tp_strings.Items[iComment].iEnd < text_length-2)
            )
            then begin
              textnostrings := textnostrings + Copy(self.text^, tp_strings.Items[iComment].iEnd, (text_length - tp_strings.Items[iComment].iEnd) + 1);
            end;
          end;
        end;

        // This is more efficient than copying
        self.text^ := textnostrings;  // Note: In C++Builder there is a swap() function which is much faster. When it will be available in Delphi too, use it then.
      end;
    end;
  end;

  //****************************************************************
  //**              Remove non PHP areas (like HTML)              **
  //****************************************************************
  tp_strings.Clear();
  bInPHPCode := false;
  iStart := 0;
  iEnd := 0;
  j := 0;

  repeat
    if (not bInPHPCode) then
    begin
      j := Pos('<?php', self.text^, j+1);

      if j > 0 then
      begin
        bInPHPCode := true;
        iStart := j;
      end;
    end
    else
    begin
      j := Pos('?>', self.text^, j+1);

      if j > 0 then
      begin
        iEnd := j+2;
        tp_strings.Add(Ttextpos.Create(iStart, iEnd));
        bInPHPCode := false;
      end
      else  // Zend / PHP explicitly forbids the usage of the closing PHP tag "?>", so it might not be in the script! Also a non-closed string or comment might cause the closing PHP tag to be removed before this is even reached.
      begin
        iEnd := Length(self.text^)+1;  // TODO: Bug which causes plugin to crash?
        tp_strings.Add(Ttextpos.Create(iStart, iEnd));
      end;
    end;
  until (j = 0);

  if (tp_strings.Count > 0) then
  begin
    if (bReplaceWithPlaceholders) then
    begin
      text_length := Length(self.text^);
      iComments := tp_strings.Count;

      for iComment := 0 to iComments-1 do
      begin
        // Copy from first position to beginning of current comment
        if (iComment = 0) then
        begin
          ReplaceTextRange(self.text^, 1, tp_strings.Items[iComment].iStart-1, ' ');
        end
        else
        begin
          // Copy from end of previous entry to beginning of current entry
          ReplaceTextRange(self.text^, tp_strings.Items[iComment-1].iEnd, tp_strings.Items[iComment].iStart-1, ' ');

          if (iComment = iComments-1) then
          begin
            // If last element, also copy from end of current comment to end of entire source string
            ReplaceTextRange(self.text^, tp_strings.Items[iComment].iEnd, text_length, ' ');
          end;
        end;
      end;
    end
    else
    begin
      // Copy PHP code areas
      // Note: Change text in any case, because we need only PHP code but some PHP files contain only HTML or other junk
      textnononphpcode := '';
      j := tp_strings.Count;
      for i := 0 to j-1 do
      begin
        textnononphpcode := textnononphpcode + Copy(self.text^, tp_strings.Items[i].iStart, tp_strings.Items[i].iEnd - tp_strings.Items[i].iStart);
      end;

      // This is more efficient than copying
      self.text^ := textnononphpcode;  // Note: In C++Builder there is a swap() function which is much faster. When it will be available in Delphi too, use it then.
    end;
  end;

  // Free (TObjectList will delete objects automatically - no need to delete or clear)
  tp_strings.Free();

  // Free some memory
  textnocomments := '';
  textnostrings := '';
  textnononphpcode := '';
end;
//---------------------------------------------------------------------------
procedure TAnalyzerPHP.Inherit();
var
  php_class: AnalyzerPHPClass;
  php_attribute: AnalyzerPHPAttribute;
  php_method: AnalyzerPHPMethod;
  iClass: Integer;
  iAttribute: Integer;
  iMethod: Integer;
  iParameter: Integer;
begin
  // Go through all classes
  for iClass := 0 to self.classes.Count-1 do
  begin
    php_class := self.classes.Items[iClass];

    while (true) do
    begin
      if
      (
        (Assigned(php_class)) and
        (php_class.derivedFromClass <> '')
      )
      then begin
        php_class := self.getClassByName(php_class.derivedFromClass);

        if not Assigned(php_class) then
        begin
          // Leave while loop (this should normally never happen here)
          //ShowMessage("inherit error: parent class not found");
          break;
        end
        else begin
          // php_class contains the parent class. Get new public & protected methods and attributes

          // Get new inheritable attributes
          for iAttribute := 0 to php_class.attributes.Count-1 do
          begin
            if
            (
              (
                (php_class.attributes.Items[iAttribute].visibility = asvPublic) or
                (php_class.attributes.Items[iAttribute].visibility = asvProtected)
              )
              and
              (not Assigned(self.getAttributeByName(self.classes.Items[iClass], php_class.attributes.Items[iAttribute].name)))  // Attribute in parent class must not exist in current class to be added
            )
            then begin
              self.classes.Items[iClass].attributes.Add(AnalyzerPHPAttribute.Create());
              php_attribute := self.classes.Items[iClass].attributes.Last;
              php_attribute.visibility := php_class.attributes.Items[iAttribute].visibility;
              php_attribute.bStatic := php_class.attributes.Items[iAttribute].bStatic;
              php_attribute.bConst := php_class.attributes.Items[iAttribute].bConst;
              php_attribute.name := php_class.attributes.Items[iAttribute].name;
            end;
          end;

          // Get new inheritable methods
          for iMethod := 0 to php_class.methods.Count-1 do
          begin
            if
            (
              (
                (php_class.methods.Items[iMethod].visibility = asvPublic) or
                (php_class.methods.Items[iMethod].visibility = asvProtected)
              )
              and
              (not Assigned(self.getMethodByName(self.classes.Items[iClass], php_class.methods.Items[iMethod].name)))  // Method in parent class must not exist in current class to be added
            )
            then begin
              self.classes.Items[iClass].methods.Add(AnalyzerPHPMethod.Create());
              php_method := self.classes.Items[iClass].methods.Last;
              php_method.visibility := php_class.methods.Items[iMethod].visibility;
              php_method.bStatic := php_class.methods.Items[iMethod].bStatic;
              php_method.name := php_class.methods.Items[iMethod].name;

              // Copy parameters (they are just strings)
              for iParameter := 0 to php_class.methods.Items[iMethod].parameters.Count-1 do
              begin
                php_method.parameters.Add(php_class.methods.Items[iMethod].parameters[iParameter]);
              end;
            end;
          end;
        end;
      end
      else begin
        break;
      end;
    end;
  end;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getClassByName(class_name: String): AnalyzerPHPClass;  // Class names are case-insensitive (no multiple classes with same name)
var
  php_class: AnalyzerPHPClass;
  iClass: Integer;
begin
  php_class := nil;

  for iClass := 0 to self.classes.Count-1 do
  begin
    if (LowerCase(self.classes.Items[iClass].name) = LowerCase(class_name)) then
    begin
      php_class := self.classes.Items[iClass];
      break;
    end;
  end;

  Result := php_class;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getMethodByName(php_class: AnalyzerPHPClass; method_name: String): AnalyzerPHPMethod;  // Methods names are case-insensitive (no multiple methods with same name)
var
  php_method: AnalyzerPHPMethod;
  iMethod: Integer;
begin
  php_method := nil;

  for iMethod := 0 to php_class.methods.Count-1 do
  begin
    if (LowerCase(php_class.methods.Items[iMethod].name) = LowerCase(method_name)) then
    begin
      php_method := php_class.methods.Items[iMethod];
      break;
    end;
  end;

  Result := php_method;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getAttributeByName(php_class: AnalyzerPHPClass; attribute_name: String): AnalyzerPHPAttribute;  // Attributes are case sensitive!
var
  php_attribute: AnalyzerPHPAttribute;
  iAttribute: Integer;
begin
  php_attribute := nil;

  for iAttribute := 0 to php_class.attributes.Count-1 do
  begin
     if (php_class.attributes.Items[iAttribute].name = attribute_name) then
    begin
      php_attribute := php_class.attributes.Items[iAttribute];
      break;
    end;
  end;

  Result := php_attribute;
end;
//---------------------------------------------------------------------------
function TAnalyzerPHP.getDBFilesIndexByFileName(filename: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to self.dbfiles.Count-1 do
  begin
    if (self.dbfiles.Items[i].sFile = filename) then
    begin
      Result := i;
      break;
    end;
  end;
end;
//---------------------------------------------------------------------------

end.
