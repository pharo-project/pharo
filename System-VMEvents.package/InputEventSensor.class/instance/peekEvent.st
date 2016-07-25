events
peekEvent
	"Look ahead at the next event."
	| nextEvent |
	nextEvent := eventQueue peek.
	^((nextEvent notNil) and: [(nextEvent at: 1) ~= EventTypeMenu])
		ifTrue: [self processEvent: nextEvent]