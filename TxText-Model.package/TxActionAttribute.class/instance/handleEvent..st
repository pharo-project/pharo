event handling
handleEvent: anEvent
	"Answer true if event passes the filter, false otherwise"
	
	(self filterEvent: anEvent) ifTrue: [ 
		value cull: anEvent.
		^ true
		 ].
	^ false
