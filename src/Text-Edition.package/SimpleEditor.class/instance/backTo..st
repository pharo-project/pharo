typing support
backTo: startIndex
	"During typing, backspace to startIndex.  Deleted characters fall into three
	 clusters, from left to right in the text: (1) preexisting characters that were
	 backed over; (2) newly typed characters that were backed over; 
	(3) preexisting characters that
	 were highlighted before typing began.  If typing has not yet been opened,
	 open it and watch for the first and third cluster.  If typing has been opened,
	 watch for the first and second cluster.  
	 Delete all the clusters.  The code is shorter than the comment."

	| saveLimit |
	saveLimit := beginTypeInIndex
		ifNil: [self openTypeIn. self stopIndex]
		ifNotNil: [self startOfTyping].
	self markIndex: startIndex.
	startIndex < saveLimit ifTrue: [
		beginTypeInIndex := self startIndex.].
	self zapSelectionWith: ''.
	self unselect