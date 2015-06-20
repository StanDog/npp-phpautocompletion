Welcome, future developer of Notepad++ plugins (in Delphi)!

This is a base framework that will help you create Notepad++ plugins.
The framework itself it stored in the 'lib' subdirectory, you mostly have to
worry about the stuff that is in the root directory. Most of the DLL
initialization has been moved to framework files, so you don't even have to
bother with this. The thing you need to do is:
 - Create a new project
 - Add all framework files to the project
 - In your project file add {$Include 'lib\NppPluginInclude.pas'} and inside begin end. add DllProc := @DLLEntryPoint; DLLEntryPoint(DLL_PROCESS_ATTACH);
 - Create a new unit that will extend NppPlugin class
 - In the interface of your plugin unit create a variable Npp of your plugin class (ex: var Npp: THelloWorldPlugin;).
 - At the end of that unit add initialization Npp := TYourExcelentPlugin.Create;
 - You can use TNppForm and TNppDockingForm as base for your forms. Remember to use the correct constructor!
 - Create your windows when the are requested by the user via one of the func calls. Notepad++ will also use this when it starts up.
 - Never create windows in the costructor or before setInfo is called.
Other stuff that might be interesting to extend in your plugin:
 - BeNotified (NPPN_TB_MODIFICATION - extend DoNppnToolbarModification, NPPN_SHUTDOWN - extend DoNppnShutdown ...)
 - MessageProc (WM_CREATE...)
Notes:
 - I'd like to make this plugin more in a way that real Delphi stuff is made (usage of ObjectInpector, not creating new constructors...), but that would require me to make an installable component. I hate this. If anyone wants to do this for me...
 - How to test your plugin:
   - Project/Options/Directories/Output Directory: C:\notepad_install_dir\Plugins
   - Run/Parameters/Host application: C:\notepad_install_dir\Notepad++.exe
   - F9
   - Note: If you want to test things on a different notepad++ version, just download the bin, put it somewhere and update the directories.

Update 2.0
 - Unicode support: Set NPPUNICODE define to enable - Project/Options/Conditionals/Conditional Defines: NPPUNICODE
 - in plugin constructor, use AddFuncItem helper instead of editing FuncArray directly
 - also use CmdIdFromDlgId instead accessing FuncArray
 - Do not override TNppPlugin.MessageProc, instead use the Delphi message directive:
     procedure TYourPlugin.WmResize(var msg:TMessage); message WM_MESSAGE;
 - Added new NPPM constants
 
Any suggestions or questions: zobo@users.sf.net
