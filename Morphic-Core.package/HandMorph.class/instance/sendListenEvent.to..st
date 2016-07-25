private events
sendListenEvent: anEvent to: listenerGroup
	"Send the event to the given group of listeners"
	listenerGroup ifNil:[^self].
	listenerGroup do:[:listener| 
		listener ifNotNil:[listener handleListenEvent: anEvent copy]].