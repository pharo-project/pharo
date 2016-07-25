event handling
mouseLeave: evt
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed. The default response is to let my eventHandler, if any, handle it."

	^ self eventHandler ifNotNil:
		[self eventHandler mouseLeave: evt fromMorph: self].
