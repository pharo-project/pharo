removing
removeFirst
	"Remove the first element from the receiver"
	self emptyCheck.
	^self privateRemoveAt: 1