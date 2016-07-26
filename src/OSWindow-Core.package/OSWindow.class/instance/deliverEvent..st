dispatching events
deliverEvent: anEvent
	"TODO..."
	
	TraceEvents == true ifTrue: [ Transcript show: anEvent; cr ].
	eventHandler ifNotNil: [ 
		eventHandler handleEvent: anEvent ].
	anEvent performDefaultAction 
