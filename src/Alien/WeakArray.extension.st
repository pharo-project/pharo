Extension { #name : #WeakArray }

{ #category : #'*Alien-Core' }
WeakArray class >> hasWeakDependent: anObject [
	^FinalizationDependents includes: anObject
]
