Extension { #name : #ShiftClassBuilder }

{ #category : #'*FluidClassBuilder' }
ShiftClassBuilder >> sharedPools [
	^ self layoutDefinition sharedPools
]

{ #category : #'*FluidClassBuilder' }
ShiftClassBuilder >> sharedPoolsCollectionOfSymbols: aCol [
	self layoutDefinition sharedPools: aCol
]

{ #category : #'*FluidClassBuilder' }
ShiftClassBuilder >> sharedVariables [
	^ self layoutDefinition sharedVariables
]

{ #category : #'*FluidClassBuilder' }
ShiftClassBuilder >> slots [
	^ self layoutDefinition slots
]
