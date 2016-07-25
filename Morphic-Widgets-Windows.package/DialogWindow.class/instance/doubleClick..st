event handling
doubleClick: event
	"Handle a double click. Maximize/restore the window.
	Not for dialogs if not resizeable..."
	
	self isResizeable ifTrue: [super doubleClick: event]