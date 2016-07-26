private
startIndexFor: anObject
	"Return the index at which the scan for anObject should start."
	
	^(anObject hash \\ array size) + 1