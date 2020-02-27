Class {
	#name : #RBRefactoringBrowserTest,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #testing }
RBRefactoringBrowserTest class >> isAbstract [
	^ self name = #RBRefactoringBrowserTest
]

{ #category : #private }
RBRefactoringBrowserTest >> executeRefactoring: aRefactoring [
	aRefactoring primitiveExecute.
	self parserClass parseExpression: aRefactoring storeString
]

{ #category : #private }
RBRefactoringBrowserTest >> objectClassVariable [
	^Object classPool keys detect: [:each | true]
]

{ #category : #helpers }
RBRefactoringBrowserTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : #helpers }
RBRefactoringBrowserTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #private }
RBRefactoringBrowserTest >> parserClass [
	^ RBParser
]

{ #category : #private }
RBRefactoringBrowserTest >> proceedThroughWarning: aBlock [ 
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]
