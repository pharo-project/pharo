removing
removeLast
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject := array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^ lastObject