event handling
doubleClickTimeout: evt
	"Handle a double-click timeout event. This message is only sent to clients that request it by sending #waitForClicksOrDrag:event: to the initiating hand in their mouseDown: method. This default implementation does nothing."

	^ self eventHandler ifNotNil:
		[self eventHandler doubleClickTimeout: evt fromMorph: self].