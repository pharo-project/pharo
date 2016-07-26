accessing-query
queryRemoveAll
	"Modify the receiver by removing all query parameters"
	
	self hasQuery
		ifTrue: [ self query removeAll ]