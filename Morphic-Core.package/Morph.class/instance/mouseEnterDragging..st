event handling
mouseEnterDragging: evt
	"Handle a mouseEnterDragging event, meaning the mouse just entered my bounds with a button pressed or laden with submorphs.  The default response is to let my eventHandler, if any, handle it, or else to do nothing."

	self eventHandler ifNotNil:
		[^ self eventHandler mouseEnterDragging: evt fromMorph: self].
