event handling
mouseEnter: evt
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed. The default response is to let my eventHandler, if any, handle it."

	^ self eventHandler ifNotNil:
		[self eventHandler mouseEnter: evt fromMorph: self].
