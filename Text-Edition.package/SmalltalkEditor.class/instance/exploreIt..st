editing keys
exploreIt: aKeyboardEvent
	"Explore the selection -- invoked via cmd-shift-I.  If there is no current selection, use the current line."
	self exploreIt.
	^ true