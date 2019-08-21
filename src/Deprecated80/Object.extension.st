Extension { #name : #Object }

{ #category : #'*Deprecated80' }
Object >> asSetElement [
	self deprecated: 'Use #asCollectionElementWrapper instead' transformWith: '`@receiver asSetElement' -> '`@receiver asCollectionElementWrapper'.
	^ self asCollectionElementWrapper
]

{ #category : #'*Deprecated80' }
Object >> enclosedSetElement [
	self deprecated: 'Use #enclosedElement instead' transformWith: '`@receiver enclosedSetElement' -> '`@receiver enclosedElement'.
	^ self enclosedElement
]
