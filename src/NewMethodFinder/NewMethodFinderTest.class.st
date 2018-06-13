Class {
	#name : #NewMethodFinderTest,
	#superclass : #TestCase,
	#category : #NewMethodFinder
}

{ #category : #tests }
NewMethodFinderTest >> testBooleanAsBit [

	| res |
	res := (NewMethodFinder new 	
		guess: { true . false }  and: { 1 . 0}).
	self assert: (res includes: #asBit).
	self assert: res size equals: 1.
	
]

{ #category : #tests }
NewMethodFinderTest >> testBooleanNot [

	| res |
	res := (NewMethodFinder new 	
		guess: { true . false}  and: { false . true} ).
	self assert: (res includes: #not).
	self assert: res size equals: 1.
	
]

{ #category : #tests }
NewMethodFinderTest >> testGuess [

	| dum res |
	dum := NewMethodFinderDummy new.
	res := (NewMethodFinder new 	
		guess: { dum }  and: { dum } ).
	self assert: (res includes: #foo).
	self assert: res size equals: 1
]

{ #category : #tests }
NewMethodFinderTest >> testSearchOnDifferentClass [

	| dum foo res |
	dum := NewMethodFinderDummy new.
	foo := NewMethodFinderFoo new.
	res := (NewMethodFinder new 	
		guess: { dum . foo }  and: { dum . foo } ).
	self assert: (res isEmpty).
]
