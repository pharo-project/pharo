"
A simple trait used in tests
"
Trait {
	#name : #THEOneTestTrait,
	#category : 'Hermes-Tests-Test-Classes'
}

{ #category : #'as yet unclassified' }
THEOneTestTrait classSide >> someClassSideMessage [
	^ self new.
]

{ #category : #'a protocol so useful' }
THEOneTestTrait >> oneMessage [
	^ 1234
]

{ #category : #'a protocol so useful' }
THEOneTestTrait >> oneMessageRequired [
	self shouldBeImplemented
]
