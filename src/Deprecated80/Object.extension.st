Extension { #name : #Object }

{ #category : #'*Deprecated80' }
Object >> asSetElement [
	self deprecated: 'Use #asCollectionElement instead' transformWith: '`@receiver asSetElement' -> '`@receiver asCollectionElement'.
	^ self asCollectionElement
]

{ #category : #'*Deprecated80' }
Object >> enclosedSetElement [
	self deprecated: 'Use #enclosedElement instead' transformWith: '`@receiver enclosedSetElement' -> '`@receiver enclosedElement'.
	^ self enclosedElement
]
