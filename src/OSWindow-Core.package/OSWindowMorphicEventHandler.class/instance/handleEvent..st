events
handleEvent: anEvent
	
	"convert the event to morphic one, and dispatch it whatever..."
	| morphicEvent |
	
	morphicEvent := anEvent accept: self.
	morphicEvent isMorphicEvent ifFalse: [ ^ self ].
	
	self flag: #todo. "Why this was in a queue who was never called?"
	"self enqueue: morphicEvent."
	self dispatchMorphicEvent: morphicEvent
	