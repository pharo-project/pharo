event handling
mouseEnterDragging: evt
	"Handle a mouseEnterDragging event, meaning the mouse just entered my bounds with a button pressed or laden with submorphs."

	super mouseEnterDragging: evt.
	self over: true