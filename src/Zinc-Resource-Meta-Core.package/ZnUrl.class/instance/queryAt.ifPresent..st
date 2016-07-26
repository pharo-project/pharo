accessing-query
queryAt: key ifPresent: block
	"Execute block with the value of the query parameter named key as value.
	Return nil if there is no such parameter"
	
	^ self hasQuery
		ifTrue: [ self query at: key asString ifPresent: block ]