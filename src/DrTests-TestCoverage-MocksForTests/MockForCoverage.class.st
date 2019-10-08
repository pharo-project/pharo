Class {
	#name : #MockForCoverage,
	#superclass : #Object,
	#category : #'DrTests-TestCoverage-MocksForTests'
}

{ #category : #'as yet unclassified' }
MockForCoverage >> method1ForMock [
	^ 1 + 1
]

{ #category : #'as yet unclassified' }
MockForCoverage >> method2ForMock: anObject [
	^ 1 + anObject
]
