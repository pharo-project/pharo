events
mouseDown: evt
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	evt shiftPressed ifTrue: [^ super mouseDown: evt].  "enable label editing"
	evt hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self event: evt. 
