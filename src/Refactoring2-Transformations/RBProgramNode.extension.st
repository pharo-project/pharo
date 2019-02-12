Extension { #name : #RBProgramNode }

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> allAccesses [
	self flag: #Clean. "allAccesses is returning all the variables not whether the variable is read or set. 
	We should rename this method to allVariables"
	^ self allChildren select: [ :each | each isVariable ]
]

{ #category : #'*Refactoring2-Transformations' }
RBProgramNode >> parentSuchAs: aPredicateSelector [
	self flag: #Bogus. "either there is a precondition stating that there is a parent matching or we need to check and return something when there is no parent marching."
	^ (self parent perform: aPredicateSelector)
		ifTrue: [ self parent ]
		ifFalse: [ ^ self parent parentSuchAs: aPredicateSelector ]
]
