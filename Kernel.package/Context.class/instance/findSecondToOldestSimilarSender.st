query
findSecondToOldestSimilarSender
	"Search the stack for the second-to-oldest occurance of self's method.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning."

	| secondContext context lastContext |
	secondContext := self.
	context := self.
	[	lastContext := context findSimilarSender.
		lastContext isNil
	] whileFalse: [
		secondContext := context.
		context := lastContext.
	].
	^ secondContext
