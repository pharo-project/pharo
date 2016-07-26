morphic
uiThemeClass: aUIThemeClass
	| themeClass |
	themeClass := aUIThemeClass 
		ifNil: [ UITheme standardThemeClass ]. 
	 themeClass beCurrent.