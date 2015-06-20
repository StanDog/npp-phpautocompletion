//****************************************************************
//**       Auto completion for custom PHP classes (ACCPC)       **
//**                                                            **
//**           Written by Stanislav Eckert, 2013-2015           **
//**         Base plugin template by Damjan Zobo Cvetko         **
//****************************************************************

library ccc;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{$R 'cccres.res' 'cccres.rc'}

uses
  SysUtils,
  Classes,
  Types,
  Windows,
  Messages,
  VirtualTrees,
  ccchelperfunctions in 'ccchelperfunctions.pas',
  nppplugin in 'lib\nppplugin.pas',
  SciSupport in 'lib\SciSupport.pas',
  NppForms in 'lib\NppForms.pas' {NppForm},
  NppDockingForms in 'lib\NppDockingForms.pas' {NppDockingForm},
  cccplugin in 'cccplugin.pas',
  AboutForms in 'AboutForms.pas' {AboutForm},
  ParsingIndicatorForms in 'ParsingIndicatorForms.pas' {ParsingIndicatorForm},
  ClassesListDockingForms in 'ClassesListDockingForms.pas' {ClassesListDockingForm},
  AnalyzerPHP in 'AnalyzerPHP.pas',
  accpc_popup_helper_class in 'accpc_popup_helper_class.pas',
  CRC in 'CRC.pas';

{$R *.res}

{$Include 'lib\NppPluginInclude.pas'}

begin
  { First, assign the procedure to the DLLProc variable }
  DllProc := @DLLEntryPoint;
  { Now invoke the procedure to reflect that the DLL is attaching to the process }
  DLLEntryPoint(DLL_PROCESS_ATTACH);
end.
