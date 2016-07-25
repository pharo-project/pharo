events-processing
processEvent: anEvent using: defaultDispatcher
"	| evt |
	evt := anEvent transformedBy: owner transform.

	Transcript show: 'event pos: '; show: anEvent position;cr."
"	Transcript show: bounds;cr."
	
	"evt := anEvent transformedBy: self transform."
	
	"^ #rejected."
	^ super processEvent: anEvent using: defaultDispatcher