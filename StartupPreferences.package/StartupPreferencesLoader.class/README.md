StartupPreferencesLoader searches for and executes .st files from certain locations such as Library/Preferences/pharo on Mac OS X.  

StartupPreferencesLoader looks within such locations for a 'pharo' folder. This contains the startup scripts common to all versions of Pharo, and also optionally a folder per Pharo version holding startup scripts suitable for that version only.  So a typical directory layout might be...

.../some/folders/pharo/Content/Resources/pharo.image.
.../some/folders/pharo/Content/Resources/startup.st
.../some/folders/.config/pharo/author.st
.../some/folders/.config/pharo/useSharedCache.st
.../some/folders/.config/pharo/1.4/mystartupFor14only.st
.../some/folders/.config/pharo/2.0/mystartupFor20only.st

(**Note however that '.config' is an invalid filename on Windows, so '..config' is used instead)

To know the real values for you...
Print the result of "StartupPreferencesLoader preferencesGeneralFolder" which holds the startup scripts common to all versions of Pharo.
Print the result of "StartupPreferencesLoader preferencesVersionFolder" which holds the startup scripts specific to the version of the current image.

-----------


StartupPreferencesLoader example

will define a script sample startup.st in your unix root on unix 

Its contents is 

StartupPreferencesLoader default executeAtomicItems: {
	StartupAtomicItem name: 'Open Help' code: 'Workspace openContents: ''Here is just an example of how to use the StartupPreferencesLoader.
I should only be displayed once.
	
You can also see StartupPreferencesLoader class>>#example'' label: ''Help''' isSingleton: true.
	StartupAtomicItem name: 'Open Workspace' code: 'Workspace openContents: ''I should be displayed each time'''.
}

EXAMPLE 1
============

	" an example of script generation "
	| item1 item2 |
	item1 := StartupAction name: 'Open Help' code: 'Smalltalk tools workspace openContents: ''Here is just an example of how to use the StartupPreferencesLoader.
I should only be displayed once.
	
 You can also see StartupPreferencesLoader class>>#example'' label: ''Help''' runOnce: true.
	item2 := StartupAction name: 'Open Workspace' code:  [ Workspace openContents: 'I should be displayed each time' ].
	StartupPreferencesLoader default addAtStartupInGeneralPreferenceFolder: {item1. item2}.
	
	StartupPreferencesLoader default loadFromDefaultLocations.
	StartupPreferencesLoader default loadFromDefaultLocations.

EXAMPLE 2
============

	" it's my personal script provided as example"
	| items |
	items := OrderedCollection new.
	items add: (StartupAction name: 'Set the Author' code: [ Author fullName: Author fullName printString ]). "replace it by your name"
	
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'author.st'.
	items removeAll.
	
	items add: (StartupAction name: 'Debugger option' code: [ Smalltalk tools debugger alwaysOpenFullDebugger: true ]).
	items add: (StartupAction name: 'Dragging Option' code: [ UITheme defaultSettings fastDragging: true ]).
	items add: (StartupAction name: 'Dialog Auto Accept' code: [ TextEditorDialogWindow autoAccept: true ]).
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'settings.st'.
	items removeAll.
	
	items add: (StartupAction name: 'Fonts option' code: [ FreeTypeSystemSettings loadFt2Library: true.	
	FreeTypeFontProvider current updateFromSystem.
	StandardFonts defaultFont: (LogicalFont familyName: 'Lucida Grande' pointSize: 10) forceNotBold.
	GraphicFontSettings resetAllFontToDefault.
	StandardFonts codeFont: (LogicalFont familyName: 'Consolas' pointSize: 10).] runOnce: true).
	
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'fonts.st'.
	items removeAll.
	StartupPreferencesLoader default addAtStartupInImageDirectory: items.
	
	StartupPreferencesLoader default loadFromDefaultLocations.

EXAMPLE 3
============

	| items |
	items := OrderedCollection new.
	
	items add: (StartupAction name: 'General Preferences for all Pharo versions' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting general preferences for all Pharo versions'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInGeneralPreferenceFolder: items named: 'generalSettings.st'.
	
	items removeAll.
	items add: (StartupAction name: 'Settings' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting general preferences for Pharo 2.0'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'settings.st'.
	
	items removeAll.
	items add: (StartupAction name: 'Image diretory' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting preferences for image directory'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInImageDirectory: items 