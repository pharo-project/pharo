event handling
mouseDown: event
	"If pane is not empty, pass the event to the last submorph,
	assuming it is the most appropriate recipient (!)"

	(self innerBounds containsPoint: event cursorPoint)
		ifTrue: [ 
			self scrollPane mouseDown: event.
			self eventHandler ifNotNil: [ self eventHandler mouseDown: event fromMorph: self ] ]