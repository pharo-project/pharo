Extension { #name : #RBProgramNode }

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> allAccesses [
	self deprecated: 'Use #allVariables instead' transformWith: '`@receiver allAccesses' -> '`@receiver allVariables'.
	^ self allVariables
]

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> parentSuchAs: aPredicateSelector [
	self flag: #pharoFixMe. "either there is a precondition stating that there is a parent matching or we need to check and return something when there is no parent matching."
	^ (self parent perform: aPredicateSelector)
		ifTrue: [ self parent ]
		ifFalse: [ ^ self parent parentSuchAs: aPredicateSelector ]
]
