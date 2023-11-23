"
A mock class with methods used for testing the coverage plugin of DrTest
"
Class {
	#name : 'MockForCoverage',
	#superclass : 'Object',
	#category : 'DrTests-TestCoverage-Tests-Mocks-Mocking',
	#package : 'DrTests-TestCoverage-Tests-Mocks',
	#tag : 'Mocking'
}

{ #category : 'sample methods' }
MockForCoverage >> method1ForMock [

	^ 1 + 1
]

{ #category : 'sample methods' }
MockForCoverage >> method2ForMock: anObject [

	^ 1 + anObject
]

{ #category : 'sample methods' }
MockForCoverage >> method3ForMockWithConditional: anInteger [

	anInteger > 5
		ifTrue: [ ^ 1 ]
		ifFalse: [ ^ 0 ]
]
