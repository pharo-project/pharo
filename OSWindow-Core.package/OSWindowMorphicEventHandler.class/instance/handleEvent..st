events
handleEvent: anEvent
	
	"convert the event to morphic one, and dispatch it whatever..."
	| morphicEvent |
	
	morphicEvent := anEvent accept: self.
	morphicEvent isMorphicEvent 
		ifTrue: [ self enqueue: morphicEvent ]
	
	