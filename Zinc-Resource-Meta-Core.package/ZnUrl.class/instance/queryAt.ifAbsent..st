accessing-query
queryAt: key ifAbsent: block
	"Return the value of the query parameter key in the receiver.
	Execute block if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifAbsent: block ]
		ifFalse: block