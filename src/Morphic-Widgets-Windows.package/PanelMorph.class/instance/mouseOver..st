protocol
mouseOver: anEvent
	"Handle a mouseOver event, meaning the mouse just moved within the receiver
	with no button pressed. The default response is to let my eventHandler, if any, handle it."

	self eventHandler ifNotNil:
		[self eventHandler mouseOver: anEvent fromMorph: self]