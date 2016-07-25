removing
removeFirst
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject := array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^ firstObject