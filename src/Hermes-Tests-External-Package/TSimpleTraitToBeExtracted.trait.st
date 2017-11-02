Trait {
	#name : #TSimpleTraitToBeExtracted,
	#category : 'Hermes-Tests-External-Package'
}

{ #category : #'as yet unclassified' }
TSimpleTraitToBeExtracted >> anObject [
	^ self shouldBeImplemented
]

{ #category : #'as yet unclassified' }
TSimpleTraitToBeExtracted >> doingSomething [
	^ self anObject
]
