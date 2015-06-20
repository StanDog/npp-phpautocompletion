//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

unit ClassesListDockingForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppDockingForms, Vcl.StdCtrls, NppPlugin, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ImgList, ccchelperfunctions, AnalyzerPHP, VirtualTrees,
  SciSupport, System.WideStrUtils;

type
  TClassesListDockingForm = class(TNppDockingForm)
    StatusBar: TStatusBar;
    edtSearch: TLabeledEdit;
    cbxShowPrivateProtected: TCheckBox;
    cbxShowSeparately: TCheckBox;
    TreeViewImageList: TImageList;
    ClassLinkingListStartTimer: TTimer;
    ClassListTreeView: TVirtualStringTree;
    cbxShowMethodParameters: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormHide(Sender: TObject);
    procedure FormFloat(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxShowPrivateProtectedClick(Sender: TObject);
    procedure cbxShowSeparatelyClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure ClassLinkingListStartTimerTimer(Sender: TObject);
    procedure ClassListTreeViewGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure ClassListTreeViewGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ClassListTreeViewNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure ClassListTreeViewDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure cbxShowMethodParametersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DockingFormActive: Boolean;
    procedure BuildClassLinkingList(var analyzer: TAnalyzerPHP);
  end;

rTreeData = record
  nodeType: Integer;  // 1=class node, 2=file entry, 3=attribute and/or method node, 4=attribute entry, 5=method entry
  ImageIndex: Integer;
  Value: String;
  Text: String;
  // Spezialized settings
  Visibility: as_visibility;
  classNodeMoved: Boolean;  // Needed while moving temporary classes to top of the list
  classIndex: Integer;
  methodIndex: Integer;
  fileOffset: UINT32;
end;

var
  ClassesListDockingForm: TClassesListDockingForm;

implementation

uses
  cccplugin;

{$R *.dfm}
//---------------------------------------------------------------------------
function CountVisibleChildNodes(list: TVirtualStringTree; node: PVirtualNode): Integer;
begin
  Result := 0;
  node := node.FirstChild;

  while (node <> nil) do
  begin
    if (list.IsVisible[node]) then
      Inc(Result);

    node := node.NextSibling;
  end;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.cbxShowPrivateProtectedClick(Sender: TObject);
var
  Node1, Node2, Node3: PVirtualNode;
  NodeData: ^rTreeData;
begin

  // Save setting
  WritePrivateProfileString('settings', 'ClassLinkingList_ShowPrivateProtected', PWideChar(String(iif(self.cbxShowPrivateProtected.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));

  // Update list without rebuilding it (just make entries visible)
  Node1 := ClassListTreeView.RootNode.FirstChild;
  while (Node1 <> nil) do
  begin
    Node2 := Node1.FirstChild;

    while (Node2 <> nil) do
    begin
      NodeData := ClassListTreeView.GetNodeData(Node2);

      if (NodeData.nodeType = 3) then
      begin
        // Show or hide attributes / methods
        Node3 := Node2.FirstChild;
        while (Node3 <> nil) do
        begin
          NodeData := ClassListTreeView.GetNodeData(Node3);
          if
          (
            (NodeData.Visibility = asvPrivate) or
            (NodeData.Visibility = asvProtected)
          )
          then begin
            ClassListTreeView.IsVisible[Node3] := cbxShowPrivateProtected.Checked;
          end;

          Node3 := Node3.NextSibling;
        end;

        // Show or hide entire attribute/method node
        ClassListTreeView.IsVisible[Node2] := (CountVisibleChildNodes(ClassListTreeView, Node2) > 0);
      end;

      Node2 := Node2.NextSibling;
    end;

    Node1 := Node1.NextSibling;
  end;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.cbxShowSeparatelyClick(Sender: TObject);
begin
  inherited;

  // Save setting
  WritePrivateProfileString('settings', 'ClassLinkingList_ShowSeparated', PWideChar(String(iif(self.cbxShowSeparately.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));

  // Rebuild list
  BuildClassLinkingList(php_analyzer);

end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.cbxShowMethodParametersClick(Sender: TObject);
begin
  inherited;

  // Save setting
  WritePrivateProfileString('settings', 'ClassLinkingList_ShowMethodParameters', PWideChar(String(iif(self.cbxShowSeparately.Checked, '1', '0'))), PWideChar(settingsPath + settingsFile));

  // Rebuild list
  BuildClassLinkingList(php_analyzer);

end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.ClassLinkingListStartTimerTimer(Sender: TObject);
var
  iOKLoad: Integer;
  settingsVersion: UINT32;
begin
  inherited;

  // Wait until plugin initialized fully and then rebuild class linking list
  if (bInitialized) then
  begin
    // Disable timer
    ClassLinkingListStartTimer.Enabled := false;

    // Load settings. Note: We need to do this here, since NPP calls the function to create the docking form before plugin fully initialized (TODO: Check)
    iOKLoad := IDYES;
    settingsVersion := GetPrivateProfileInt('settings', 'version', iAppVersion, PWideChar(settingsPath + settingsFile));
    if not settingsCompatibleVersions.Contains(settingsVersion) then
    begin
      iOKLoad := Application.MessageBox('Settings file seems to be incompatible.'+ #10 +'Loading it may mess things up.'+ #10#10 +'Do you want to try to load it?', 'ACCPC - Loading configuration file', 4+48);
    end;

    if (iOKLoad = IDYES) then
    begin
      self.cbxShowPrivateProtected.Checked := Boolean(GetPrivateProfileInt('settings', 'ClassLinkingList_ShowPrivateProtected', 0, PWideChar(settingsPath + settingsFile)));
      self.cbxShowSeparately.Checked := Boolean(GetPrivateProfileInt('settings', 'ClassLinkingList_ShowSeparated', 1, PWideChar(settingsPath + settingsFile)));
      self.cbxShowMethodParameters.Checked := Boolean(GetPrivateProfileInt('settings', 'ClassLinkingList_ShowMethodParameters', 1, PWideChar(settingsPath + settingsFile)));
    end;

    // Rebuild list
    BuildClassLinkingList(php_analyzer);
  end;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.ClassListTreeViewDrawText(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect;
  var DefaultDraw: Boolean);
var
  NodeData1, NodeData2: ^rTreeData;
  iDisplayedShiftX: Integer;
  nodeSelected: Boolean;
  i: Integer;
  parameters: ^TStringList;
begin
  inherited;
  NodeData1 := Sender.GetNodeData(Node);
  nodeSelected := ClassListTreeView.Selected[Node];

  if (not php_analyzer.isAnalyzing) then
  begin
    // Draw method nodes separately (because of optional parameters)
    // Note: Currently right-to-left texts are not supported in the list (like in autocompletion popup). They will be shown from left-to-right like other texts.
    if
    (
      (NodeData1.nodeType = 5) and
      (cbxShowMethodParameters.Checked)
    )
    then begin
      NodeData2 := Sender.GetNodeData(Node.Parent.Parent);
      parameters := @php_analyzer.classes.Items[NodeData2.classIndex].methods.Items[NodeData1.methodIndex].parameters;

      // Prevent list from drawing the text
      DefaultDraw := false;

      // Draw name of method (+ parameters)
      TargetCanvas.Brush.Style := bsClear;
      iDisplayedShiftX := CellRect.Left;

      if Pos('(', Text) > 0 then
      begin
        // Draw brackets with parameters
        TargetCanvas.Font.Color := iif(nodeSelected, clHighlightText, clWindowText);
        TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, Copy(Text, 1, Pos('(', Text)));  // Draw with bracket
        iDisplayedShiftX := iDisplayedShiftX + TargetCanvas.TextWidth(Copy(Text, 1, Pos('(', Text)));

        for i := 0 to parameters.Count-1 do
        begin
          if Pos('=', parameters.Strings[i]) > 0 then
          begin
            TargetCanvas.Font.Color := iif(nodeSelected, clLtGray, clGray);
            TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, TrimRight(Copy(parameters.Strings[i], 1, Pos('=', parameters.Strings[i])-1)));
            iDisplayedShiftX := iDisplayedShiftX + TargetCanvas.TextWidth(TrimRight(Copy(parameters.Strings[i], 1, Pos('=', parameters.Strings[i])-1)));
          end
          else
          begin
            TargetCanvas.Font.Color := iif(nodeSelected, clHighlightText, clWindowText);
            TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, parameters.Strings[i]);
            iDisplayedShiftX := iDisplayedShiftX + TargetCanvas.TextWidth(parameters.Strings[i]);
          end;

          if (i < parameters.Count-1) then
          begin
            TargetCanvas.Font.Color := iif(nodeSelected, clHighlightText, clWindowText);
            TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, ', ');
            iDisplayedShiftX := iDisplayedShiftX + TargetCanvas.TextWidth(', ');
          end;
        end;

        // Draw last bracket
        TargetCanvas.Font.Color := iif(nodeSelected, clHighlightText, clWindowText);
        TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, ')');  // Draw with bracket
      end
      else
      begin
        TargetCanvas.Font.Color := clWindowText;
        TargetCanvas.TextOut(iDisplayedShiftX, CellRect.Top + 2, Text);
      end;
    end;
  end;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.ClassListTreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: ^rTreeData;
begin
  NodeData := Sender.GetNodeData(Node);
  ImageIndex := NodeData.ImageIndex;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.ClassListTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: ^rTreeData;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData.Text;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.ClassListTreeViewNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  fileNode: PVirtualNode;
  NodeData: ^rTreeData;
  fileOffset: UINT32;
  classFile: String;
  classFileSL: TStringList;
  classFileS: String;
  classFileSUTF8: UTF8String;
  hCurrentEditView: HWND;
begin
  inherited;
  NodeData := Sender.GetNodeData(HitInfo.HitNode);

  // Double click on file node, opens the file
  if
  (
    (NodeData.nodeType = 2) and
    (NodeData.Value <> '')
  )
  then begin
    if (FileExists(NodeData.Value))
    then begin
      Npp.DoOpen(NodeData.Value);
      Windows.SetFocus(Npp.GetCurrentScintilla);
    end
    else
      Application.MessageBox(PWideChar('File "'+ NodeData.Value + '" cannot be opened, because it is not available anymore.'), 'Notepad++ - Failed to open file', 64);
  end

  // Double click on attribute / method node, opens the file and goes to position where the attribute / method was defined
  else if
  (
    (NodeData.nodeType = 4) or
    (NodeData.nodeType = 5)
  )
  then begin
    fileOffset := NodeData.fileOffset;
    classFile := '';
    fileNode := HitInfo.HitNode.Parent.Parent.FirstChild;

    // Find file name of class of attribute/method by getting the last file node
    while (fileNode <> nil) do
    begin
      NodeData := Sender.GetNodeData(fileNode);

      if
      (
        (NodeData.nodeType = 2) and  // if file node
        (NodeData.Value <> '') and
        (FileExists(NodeData.Value))
      )
      then
        classFile := NodeData.Value;

      fileNode := fileNode.NextSibling;
    end;

    if (classFile <> '') then
    begin
      // Open file
      Npp.DoOpen(classFile);

      // Get file contents (use TStringList, since easier)
      classFileSL := TStringList.Create();
      classFileSL.LoadFromFile(classFile, TEncoding.ANSI);  // TODO: Is this required or can we always load as UTF-8?
      if (IsUTF8String(classFileSL.Text)) then
        classFileSL.LoadFromFile(classFile, TEncoding.UTF8);
      classFileS := classFileSL.Text;
      classFileSL.Free();

      // Calculate ANSI Pos of Unicode Pos
      SetLength(classFileS, fileOffset - 1);
      SetLength(classFileSUTF8, Length(classFileS) * SizeOf(char) + 1);
      fileOffset := UnicodeToUtf8(PAnsiChar(classFileSUTF8), Length(classFileSUTF8), PWideChar(classFileS), Length(classFileS));
      fileOffset := fileOffset - 1;  // since VCL strings start with 1 and not 0

      // Tell Scintilla to jump to the offset
      hCurrentEditView := Npp.GetCurrentScintilla();
      SendMessage(hCurrentEditView, SciSupport.SCI_GOTOPOS, fileOffset, 0);
      Windows.SetFocus(hCurrentEditView);
    end;
  end;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.edtSearchChange(Sender: TObject);
begin
  inherited;

  // Rebuild list
  BuildClassLinkingList(php_analyzer);

end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.FormCreate(Sender: TObject);
begin
  self.NppDefaultDockingMask := DWS_DF_CONT_RIGHT;  // whats the default docking position
  self.KeyPreview := true;  // special hack for input forms
  self.OnFloat := self.FormFloat;
  self.OnDock := self.FormDock;
  self.DockingFormActive := false;

  // Load image list
  ILAddMasked(self.TreeViewImageList, 'CLDF_ATTRIBUTEMETHOD', clLime);
  ILAddMasked(self.TreeViewImageList, 'CLDF_CLASS', clLime);
  ILAddMasked(self.TreeViewImageList, 'CLDF_FILE', clLime);
  ILAddMasked(self.TreeViewImageList, 'CLDF_PRIVATE', clLime);
  ILAddMasked(self.TreeViewImageList, 'CLDF_PROTECTED', clLime);
  ILAddMasked(self.TreeViewImageList, 'CLDF_PUBLIC', clLime);

  // Tell virtual tree view the size of node data record
  ClassListTreeView.NodeDataSize := SizeOf(rTreeData);

  inherited;
end;
//---------------------------------------------------------------------------
// special hack for input forms
// This is the best possible hack I could came up for
// memo boxes that don't process enter keys for reasons
// too complicated... Has something to do with Dialog Messages
// I sends a Ctrl+Enter in place of Enter
procedure TClassesListDockingForm.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  //if (Key = #13) and (self.ClassListTreeView.Focused) then self.ClassListTreeView.Perform(WM_CHAR, 10, 0);
end;
//---------------------------------------------------------------------------
// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TClassesListDockingForm.FormHide(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 0);
  self.DockingFormActive := false;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.FormDock(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.FormFloat(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.FormShow(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
  self.DockingFormActive := true;
end;
//---------------------------------------------------------------------------
procedure TClassesListDockingForm.BuildClassLinkingList(var analyzer: TAnalyzerPHP);
var
  Node1, Node2, Node3, tmpNode1, tmpNode2, tmpNode3: PVirtualNode;
  NodeData: ^rTreeData;
  i, j, k, l: Integer;
  sSearchText: String;
  sFullMethodString: String;
  bFoundAttributeOrMethodNode: Boolean;
begin

  ClassListTreeView.BeginUpdate();

  // Clear list
  ClassListTreeView.Clear();

  // Build list
  for i := 0 to analyzer.classLinkingList.Count-1 do
  begin

    // Create class node
    Node1 := ClassListTreeView.AddChild(nil);
    NodeData := ClassListTreeView.GetNodeData(Node1);
    NodeData.Text := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].name;
    NodeData.nodeType := 1;
    NodeData.Value := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].name;
    NodeData.ImageIndex := 1;
    NodeData.classNodeMoved := false;
    NodeData.classIndex := analyzer.classLinkingList.Items[i].classIndex;
    NodeData.fileOffset := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].fileOffset;

    // Create file entries in which class is defined
    for j := 0 to analyzer.classLinkingList.Items[i].fileIndices.Count-1 do
    begin
      if (analyzer.classLinkingList.Items[i].fileIndices.Items[j] = -1) then
      begin
        Node2 := ClassListTreeView.AddChild(Node1);
        NodeData := ClassListTreeView.GetNodeData(Node2);
        NodeData.Text := 'File: <Current View>';
        NodeData.nodeType := 2;
        NodeData.Value := '';
        NodeData.ImageIndex := 2;
      end
      else
      begin
        Node2 := ClassListTreeView.AddChild(Node1);
        NodeData := ClassListTreeView.GetNodeData(Node2);
        NodeData.Text := 'File: ' + (*ExtractShortPathName*)(analyzer.dbfiles.Items[analyzer.classLinkingList.Items[i].fileIndices.Items[j]].sFile);
        NodeData.nodeType := 2;
        NodeData.Value := analyzer.dbfiles.Items[analyzer.classLinkingList.Items[i].fileIndices.Items[j]].sFile;
        NodeData.ImageIndex := 2;
      end;
    end;

    // Create attribute node
    Node2 := ClassListTreeView.AddChild(Node1);
    NodeData := ClassListTreeView.GetNodeData(Node2);
    if (ClassesListDockingForm.cbxShowSeparately.Checked) then
    begin
      NodeData.Text := 'Attributes';
      NodeData.nodeType := 3;
      NodeData.Value := '';
      NodeData.ImageIndex := 0;
    end
    else
    begin
      NodeData.Text := 'Attributes & Methods';
      NodeData.nodeType := 3;
      NodeData.Value := '';
      NodeData.ImageIndex := 0;
    end;

    // Create attribute entries
    for j := 0 to analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Count-1 do
    begin
      Node3 := ClassListTreeView.AddChild(Node2);
      NodeData := ClassListTreeView.GetNodeData(Node3);
      NodeData.Text := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].name;
      NodeData.nodeType := 4;
      NodeData.Value := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].name;
      NodeData.Visibility := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].visibility;
      NodeData.fileOffset := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].fileOffset;

      case analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].visibility of
        asvPublic: NodeData.ImageIndex := 5;
        asvProtected: NodeData.ImageIndex := 4;
        asvPrivate: NodeData.ImageIndex := 3;
        else NodeData.ImageIndex := 5;
      end;

      // Also append if static or constant
      if
      (
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].bStatic) or
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].bConst)
      )
      then begin
        NodeData.Text := NodeData.Text + '  [';

        if (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].bStatic) then
        begin
          NodeData.Text := NodeData.Text + 'static';

          if (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].bConst) then
            NodeData.Text := NodeData.Text + ', constant';
        end
        else
        begin
          if (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].bConst) then
            NodeData.Text := NodeData.Text + 'constant';
        end;

        NodeData.Text := NodeData.Text + ']';
      end;

      // Hide node
      if
      (
        (not cbxShowPrivateProtected.Checked) and
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].visibility <> asvPublic) and
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].attributes.Items[j].visibility <> asvUnknown)
      )
      then begin
        ClassListTreeView.IsVisible[Node3] := false;
      end;
    end;

    // Hide entire attribute node if no (visible) attributes found.
    // Do this only if separated attributes and methods (if together, then this will be done later below)
    if
    (
      (CountVisibleChildNodes(ClassListTreeView, Node2) = 0) and
      (ClassesListDockingForm.cbxShowSeparately.Checked)
    )
    then begin
      ClassListTreeView.IsVisible[Node2] := false;
    end;

    // Create method node
    if (ClassesListDockingForm.cbxShowSeparately.Checked) then
    begin
      Node2 := ClassListTreeView.AddChild(Node1);
      NodeData := ClassListTreeView.GetNodeData(Node2);
      NodeData.Text := 'Methods';
      NodeData.nodeType := 3;
      NodeData.Value := '';
      NodeData.ImageIndex := 0;
    end;

    // Create method entries
    for j := 0 to analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Count-1 do
    begin
      Node3 := ClassListTreeView.AddChild(Node2);
      NodeData := ClassListTreeView.GetNodeData(Node3);
      NodeData.Text := '';  // see below
      NodeData.nodeType := 5;
      NodeData.Value := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].name;
      NodeData.Visibility := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].visibility;
      NodeData.methodIndex := j;
      NodeData.fileOffset := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].fileOffset;

      case analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].visibility of
        asvPublic: NodeData.ImageIndex := 5;
        asvProtected: NodeData.ImageIndex := 4;
        asvPrivate: NodeData.ImageIndex := 3;
        else NodeData.ImageIndex := 5;
      end;

      // Create a prebuild string so TVirtualStringTree can calculate the text wide. Text will be colored later.
      sFullMethodString := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].name;
      sFullMethodString := sFullMethodString + '(';
      if (cbxShowMethodParameters.Checked) then
      begin
        k := analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].parameters.Count;
        for l := 0 to k-1 do
        begin
          sFullMethodString := sFullMethodString + analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].parameters.Strings[l];
          if (l < k-1) then
          begin
            sFullMethodString := sFullMethodString + ',';
          end;
        end;
      end;
      sFullMethodString := sFullMethodString + ')';
      sFullMethodString := GetFormattedMethodString(sFullMethodString);
      NodeData.Text := sFullMethodString;

      // Append if method is static
      if (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].bStatic) then
        NodeData.Text := NodeData.Text + '  [static]';

      // Hide node
      if
      (
        (not cbxShowPrivateProtected.Checked) and
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].visibility <> asvPublic) and
        (analyzer.classes.Items[analyzer.classLinkingList.Items[i].classIndex].methods.Items[j].visibility <> asvUnknown)
      )
      then begin
        ClassListTreeView.IsVisible[Node3] := false;
      end;
    end;

    // Hide entire method node (or the single node if not separated) if no (visible) methods (and attributes) found
    if (CountVisibleChildNodes(ClassListTreeView, Node2) = 0) then
    begin
      ClassListTreeView.IsVisible[Node2] := false;
    end;
  end;

  // Move temporary defined classes to the top position of list.
  // Also append an asterisk (*) to the class name (can be interpreted as unsaved classes ;-)
  Node1 := ClassListTreeView.RootNode.LastChild;
  while (Node1 <> nil) do
  begin
    NodeData := ClassListTreeView.GetNodeData(Node1);
    Node3 := Node1.PrevSibling;

    if (not NodeData.classNodeMoved) then
    begin
      // Check the file sub-nodes
      Node2 := Node1.FirstChild;
      while (Node2 <> nil) do
      begin
        NodeData := ClassListTreeView.GetNodeData(Node2);

        // If node = 2 (file) and value is empty, class is temporarily defined
        if
        (
          (NodeData.nodeType = 2) and
          (NodeData.Value = '')
        )
        then begin
          // Set classNodeMoved property of class node and append asterisk
          NodeData := ClassListTreeView.GetNodeData(Node1);
          NodeData.classNodeMoved := true;
          NodeData.Text := NodeData.Text + '*';

          // Move to top
          ClassListTreeView.MoveTo(Node1, ClassListTreeView.RootNode.FirstChild, TVTNodeAttachMode.amInsertBefore, false);

          Break;
        end;

        Node2 := Node2.NextSibling;
      end;
    end;

    Node1 := Node3;
  end;

  // Perform search
  if (edtSearch.Text <> '') then
  begin
    sSearchText := LowerCase(edtSearch.Text);

    // Iterate through all classes
    Node1 := ClassListTreeView.RootNode.LastChild;
    while (Node1 <> nil) do
    begin
      Node2 := Node1.LastChild;
      tmpNode3 := Node1.PrevSibling;

      while (Node2 <> nil) do
      begin
        NodeData := ClassListTreeView.GetNodeData(Node2);
        tmpNode2 := Node2.PrevSibling;

        // If attribute/method node
        if (NodeData.nodeType = 3) then
        begin

          // Remove attribute / method entry
          Node3 := Node2.LastChild;
          while (Node3 <> nil) do
          begin
            tmpNode1 := Node3.PrevSibling;

            NodeData := ClassListTreeView.GetNodeData(Node3);
            if (Pos(sSearchText, LowerCase(NodeData.Value)) = 0) then
              ClassListTreeView.DeleteNode(Node3);

            Node3 := tmpNode1;
          end;

          // If attribute/method node empty, remove it too
          if (CountVisibleChildNodes(ClassListTreeView, Node2) = 0) then
          begin
            ClassListTreeView.DeleteNode(Node2);
          end;

        end;

        Node2 := tmpNode2;
      end;

      // Iterate through class subnodes again. If no method/attribute nodes found
      // and class name does not match too, remove entire class node.
      bFoundAttributeOrMethodNode := false;
      Node2 := Node1.LastChild;
      while (Node2 <> nil) do
      begin
        NodeData := ClassListTreeView.GetNodeData(Node2);
        if (NodeData.nodeType = 3) then
        begin
          bFoundAttributeOrMethodNode := true;
          break;
        end;

        Node2 := Node2.PrevSibling;
      end;

      NodeData := ClassListTreeView.GetNodeData(Node1);
      if
      (
        (not bFoundAttributeOrMethodNode) and
        (Pos(sSearchText, LowerCase(NodeData.Value)) = 0)
      )
      then ClassListTreeView.DeleteNode(Node1);

      Node1 := tmpNode3;
    end;
  end;

  ClassListTreeView.EndUpdate();

  // Update status bar
  StatusBar.SimpleText := IntToStr(ClassListTreeView.RootNodeCount) + ' classes';
end;
//---------------------------------------------------------------------------

end.
