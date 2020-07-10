Extension { #name : #RBProgramNode }

{ #category : #'*Deprecated90' }
RBProgramNode >> methodComments [
	self
		deprecated: 'Please use #comments instead'
		transformWith: '`@receiver methodComments' -> '`@receiver comments'.
	^self comments
]
