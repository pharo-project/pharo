events
nextEvent
	"Return the next event from the receiver."
	| evt |

	evt := eventQueue nextOrNil.
	^ evt ifNotNil: [ self processEvent: evt ]
