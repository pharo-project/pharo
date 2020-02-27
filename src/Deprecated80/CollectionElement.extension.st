Extension { #name : #CollectionElement }

{ #category : #'*Deprecated80' }
CollectionElement >> enclosedSetElement: anObject [
	self deprecated: 'Use #enclosedElement: instead' transformWith: '`@receiver enclosedSetElement: `@argument' -> '`@receiver enclosedElement: `@argument'.
	^ self enclosedElement: anObject
]
