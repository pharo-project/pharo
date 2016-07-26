event handling
mouseUp: event
	super mouseUp: event.
	
	self stopSteppingSelector: #autoScrollView:.
	"editView scrollSelectionIntoView: evt."
	editView selectionInterval: editor selectionInterval.