Class {
	#name : #RBDataTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #tests }
RBDataTest >> someMethod [
	#'some.initializations'
]

{ #category : #tests }
RBDataTest >> testExample [
	self someMethod.
	self assert: true
]
