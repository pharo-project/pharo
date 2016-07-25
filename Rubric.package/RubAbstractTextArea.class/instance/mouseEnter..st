event handling
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."
	super mouseEnter: evt.
	self showOverEditableTextCursor
	