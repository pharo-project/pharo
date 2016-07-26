event handling
mouseMove: evt
	"Handle a mouse move event. The default response is to let my eventHandler, if any, handle it."
	self eventHandler ifNotNil:
		[self eventHandler mouseMove: evt fromMorph: self].
