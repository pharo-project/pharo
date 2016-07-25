menu actions
setWindowColor
	"Allow the user to select a new basic color for the window"

	(UIManager default chooseColor: self paneColor)
		ifNotNil: [:nc | self setWindowColor: nc].
