initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		autoSelectionColor: true;
		windowColor: self defaultWindowColor;
		selectionColor: self defaultSelectionColor;
		buttonColor: self defaultButtonColor;
		scrollbarColor: self defaultScrollbarColor;
		standardColorsOnly: false