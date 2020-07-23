Extension { #name : #Variable }

{ #category : #'*Deprecated90' }
Variable >> isArg [
	self 
		deprecated: 'Use #isArgumentVariable instead.' 
		transformWith: '`@receiver isArg' -> '`@receiver isArgumentVariable'.
	
	^ self isArgumentVariable
]

{ #category : #'*Deprecated90' }
Variable >> isGlobal [
	self 
		deprecated: 'Use #isGlobalVariable instead.' 
		transformWith: '`@receiver isGlobal' -> '`@receiver isGlobalVariable'.
	
	^self isGlobalVariable
]

{ #category : #'*Deprecated90' }
Variable >> isInstance [
	self 
		deprecated: 'Use #isInstanceVariable instead.' 
		transformWith: '`@receiver isInstance' -> '`@receiver isInstanceVariable'.
	 ^self isInstanceVariable
]

{ #category : #'*Deprecated90' }
Variable >> isLocal [
	self 
		deprecated: 'Use #isLocalVariable instead.' 
		transformWith: '`@receiver isLocal' -> '`@receiver isLocalVariable'.
]

{ #category : #'*Deprecated90' }
Variable >> isSelf [
	self 
		deprecated: 'Use #isSelfVariable instead.' 
		transformWith: '`@receiver isSelf' -> '`@receiver isSelfVariable'.

	^self isSelfVariable
]

{ #category : #'*Deprecated90' }
Variable >> isSelfOrSuper [
	self 
		deprecated: 'Use #isSelfOrSuperVariable instead.' 
		transformWith: '`@receiver isSelfOrSuper' -> '`@receiver isSelfOrSuperVariable'.

	^ self isSelfOrSuperVariable
]

{ #category : #'*Deprecated90' }
Variable >> isSpecialVariable [
	self 
		deprecated: 'Use #isReservedVariable instead.' 
		transformWith: '`@receiver isSpecialVariable' -> '`@receiver isReservedVariable'.
	
	^ self isReservedVariable
]

{ #category : #'*Deprecated90' }
Variable >> isSuper [
	self 
		deprecated: 'Use #isSuperVariable instead.' 
		transformWith: '`@receiver isSuper' -> '`@receiver isSuperVariable'.
		
	^ self isSuperVariable

]

{ #category : #'*Deprecated90' }
Variable >> isThisContext [
	self 
		deprecated: 'Use #isThisContextVariable instead.' 
		transformWith: '`@receiver isThisContext' -> '`@receiver isThisContextVariable'.

	^ self isThisContextVariable
]

{ #category : #'*Deprecated90' }
Variable >> isUndeclared [
	self 
		deprecated: 'Use #isUndeclaredVariable instead.' 
		transformWith: '`@receiver isUndeclared' -> '`@receiver isUndeclaredVariable'.
	
	^self isUndeclaredVariable
]

{ #category : #'*Deprecated90' }
Variable >> variable [
	
	self 
		deprecated: 'Use receiver directly instead.' 
		transformWith: '`@receiver binding variable' -> '`@receiver binding'.
		
	^self
]
