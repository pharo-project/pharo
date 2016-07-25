events-processing
processEvent: anEvent
	"Process the given event using the default event dispatcher."
	^self processEvent: anEvent using: self defaultEventDispatcher