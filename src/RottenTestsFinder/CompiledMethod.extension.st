Extension { #name : 'CompiledMethod' }

{ #category : '*RottenTestsFinder' }
CompiledMethod >> assertionPrimitiveMethodsCalled [
	^ self messages intersection: RottenTestsFinder assertionSelectors
]

{ #category : '*RottenTestsFinder' }
CompiledMethod >> isStaticallyCallingOneOfTheAssertMethods [
	"If, with a static analysis I am calling one of the methods in the TestAssert's 'asserting' protocol, returns true.
	Else, returns false."
	^ self assertionPrimitiveMethodsCalled isNotEmpty
]

{ #category : '*RottenTestsFinder' }
CompiledMethod >> methodsCalledIn: aCollectionOfSelector [
	^ self messages intersection: aCollectionOfSelector
]
