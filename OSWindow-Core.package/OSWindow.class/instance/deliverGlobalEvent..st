dispatching events
deliverGlobalEvent: aGlobalEvent
	"TODO..."
	
	TraceEvents == true ifTrue: [ Transcript show: aGlobalEvent; cr ].
	eventHandler ifNotNil: [ 
		eventHandler handleEvent: aGlobalEvent ].
