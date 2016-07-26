accessing-query
queryAt: key
	"Return the value of the query parameter key in the receiver.
	Signal a KeyNotFound exception if there is no such parameter"
	
	^ self queryAt: key ifAbsent: [ KeyNotFound signalFor: key printString ]