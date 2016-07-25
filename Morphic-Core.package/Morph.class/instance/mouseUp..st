event handling
mouseUp: evt
	"Handle a mouse up event. The default response is to let my eventHandler, if any, handle it."

	^ self eventHandler ifNotNil:
		[self eventHandler mouseUp: evt fromMorph: self].
