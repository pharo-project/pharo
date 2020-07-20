Extension { #name : #RBVariableNode }

{ #category : #'*Deprecated90' }
RBVariableNode >> isArg [
	self 
		deprecated: 'Use #isArgumentVariable instead.' 
		transformWith: '`@receiver isArg' -> '`@receiver isArgumentVariable'.
	
	^ self isArgumentVariable
]

{ #category : #'*Deprecated90' }
RBVariableNode >> isInstance [
	self 
		deprecated: 'Use #isInstanceVariable instead.' 
		transformWith: '`@receiver isInstance' -> '`@receiver isInstanceVariable'.
	^variable isInstanceVariable
]

{ #category : #'*Deprecated90' }
RBVariableNode >> isSpecialVariable [
	self 
		deprecated: 'Use #isReservedVariable instead.' 
		transformWith: '`@receiver isSpecialVariable' -> '`@receiver isReservedVariable'.
	
	^ self isReservedVariable
]

{ #category : #'*Deprecated90' }
RBVariableNode >> isUndeclared [
	self 
		deprecated: 'Use #isUndeclaredVariable instead.' 
		transformWith: '`@receiver isUndeclared' -> '`@receiver isUndeclaredVariable'.
	
	^self isUndeclaredVariable
]
