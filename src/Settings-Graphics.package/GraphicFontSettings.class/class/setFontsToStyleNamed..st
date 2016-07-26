fonts
setFontsToStyleNamed: aSymbol
	StandardFonts setFontsToStyleNamed: aSymbol.
	SettingBrowser refreshAllSettingBrowsers.
	EditorFindReplaceDialogWindow cleanUp.