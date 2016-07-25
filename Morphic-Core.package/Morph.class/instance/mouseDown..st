event handling
mouseDown: evt 
	"Handle a mouse down event. The default response is to let my 
	eventHandler, if any, handle it."
	self eventHandler
		ifNotNil: [self eventHandler mouseDown: evt fromMorph: self]
