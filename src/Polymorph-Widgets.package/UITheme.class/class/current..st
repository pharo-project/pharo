accessing
current: aUITheme
	"Set the current ui theme."

	Current := aUITheme.
	SystemProgressMorph reset. "reset to use new fill styles"
	ScrollBar initializeImagesCache. "reset to use new arrows"

	SHPreferences setStyleTable: aUITheme shStyleTable.
	PolymorphSystemSettings desktopColor: aUITheme desktopColor.
	NECPreferences backgroundColor: aUITheme windowColor. 
	Paragraph insertionPointColor: aUITheme caretColor.
	BalloonMorph setBalloonColorTo: aUITheme balloonBackgroundColor.
	aUITheme updateWorldDockingBars.
	
	World themeChanged.