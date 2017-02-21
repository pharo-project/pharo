accessing
current: aUITheme
	"Set the current ui theme."

	Current := aUITheme.
	SystemProgressMorph reset. "reset to use new fill styles"
	ScrollBar initializeImagesCache. "reset to use new arrows"

	self class environment at: #SHPreferences ifPresent: [:shPreferences |
		shPreferences setStyleTable: aUITheme shStyleTable. ].

	self class environment at: #PolymorphSystemSettings ifPresent: [:polymorphSystemSettings |
		polymorphSystemSettings desktopColor: aUITheme desktopColor. ].
	
	self class environment at: #NECPreferences ifPresent: [:necPreferences |
		necPreferences backgroundColor: aUITheme windowColor.  ].

	self class environment at: #Paragraph ifPresent: [:paragraph |
		paragraph insertionPointColor: aUITheme caretColor.  ].

	self class environment at: #BalloonMorph ifPresent: [:balloonMorph |
		balloonMorph setBalloonColorTo: aUITheme balloonBackgroundColor. ].

	aUITheme updateWorldDockingBars.
	World themeChanged.