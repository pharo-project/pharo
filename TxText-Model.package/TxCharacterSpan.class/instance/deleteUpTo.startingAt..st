operation
deleteUpTo: endPosition startingAt: startPosition
	"Note, startPosition span == self" 
	
	self assert: startPosition span == self.
	
	endPosition span = self ifTrue: [
		"perform a local removal"
		self removeFrom: startPosition spanPosition to: endPosition spanPosition ]
	ifFalse: [
		self removeAfter: startPosition spanPosition.
		self next: endPosition span.
		self next removeBefore: endPosition spanPosition.
	].
	^ self tryMergeAndUpdatePosition: startPosition copy 
	
