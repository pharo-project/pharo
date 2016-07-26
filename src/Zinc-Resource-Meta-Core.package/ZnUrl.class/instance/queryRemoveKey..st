accessing-query
queryRemoveKey: key
	"Modify the receiver by remove the query parameter named key.
	Do nothing if there is no such parameter"
	
	self hasQuery
		ifTrue: [ self query removeKey: key asString ifAbsent: [ ] ]