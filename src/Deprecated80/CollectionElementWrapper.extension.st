Extension { #name : #CollectionElementWrapper }

{ #category : #'*Deprecated80' }
CollectionElementWrapper >> enclosedSetElement: anObject [
	self deprecated: 'Use #enclosedElement: instead' transformWith: '`@receiver enclosedSetElement: `@argument' -> '`@receiver enclosedElement: `@argument'.
	^ self enclosedElement: anObject
]
