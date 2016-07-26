event handling
mouseStillDown: evt
	"Handle a mouse move event. The default response is to let my eventHandler, if any, handle it."

	self eventHandler ifNotNil:
		[self eventHandler mouseStillDown: evt fromMorph: self].
