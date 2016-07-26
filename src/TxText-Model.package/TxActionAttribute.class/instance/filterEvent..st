event handling
filterEvent: anEvent
	"Answer true if event passes the filter, false otherwise"
	
	filter ifNil: [ ^ false ].
	^ filter value: anEvent