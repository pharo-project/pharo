windows
grabWindow
	"Request an existing window from the user and add it."

	|windows choice|
	windows := self world visibleSystemWindows.
	choice := UIManager default
		chooseFrom: (windows collect: [:e | e labelString])
		values: windows
		lines: #()
		message: 'Choose a window to add to the organiser' translated
		title: 'Grab window' translated.
	choice ifNotNil: [self addWindow: choice]