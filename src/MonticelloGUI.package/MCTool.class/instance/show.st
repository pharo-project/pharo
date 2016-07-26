opening
show
	"Open the tool returning the window."

	modal := false.
	self window openInWorld.
	self setDefaultFocus.
	^ self window