initialization
initialize
	"initialize the state of the receiver"
	super initialize.

	growingOrRotating := false.
	self borderStyle: (SimpleBorder
		width: 2
		color: self theme settings menuSelectionColor)