Extension { #name : #RBProgramNode }

{ #category : #'*Deprecated90' }
RBProgramNode >> isThisContext [
	self 
		deprecated: 'Use #isThisContextVariable instead.' 
		transformWith: '`@receiver isThisContext' -> '`@receiver isThisContextVariable'.

	^ self isThisContextVariable
]

{ #category : #'*Deprecated90' }
RBProgramNode >> methodComments [
	self
		deprecated: 'Please use #comments instead'
		transformWith: '`@receiver methodComments' -> '`@receiver comments'.
	^self comments
]
