Extension { #name : #RBProgramNode }

{ #category : #'*Deprecated90' }
RBProgramNode >> isSelf [
	self 
		deprecated: 'Use #isSelfVariable instead.' 
		transformWith: '`@receiver isSelf' -> '`@receiver isSelfVariable'.

	^self isSelfVariable
]

{ #category : #'*Deprecated90' }
RBProgramNode >> isSelfOrSuper [
	self 
		deprecated: 'Use #isSelfOrSuperVariable instead.' 
		transformWith: '`@receiver isSelfOrSuper' -> '`@receiver isSelfOrSuperVariable'.

	^ self isSelfOrSuperVariable
]

{ #category : #'*Deprecated90' }
RBProgramNode >> isSuper [
	self 
		deprecated: 'Use #isSuperVariable instead.' 
		transformWith: '`@receiver isSuper' -> '`@receiver isSuperVariable'.
		
	^ self isSuperVariable
]

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
