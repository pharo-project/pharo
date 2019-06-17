Class {
	#name : #RBRefactoringBrowserTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #testing }
RBRefactoringBrowserTest class >> isAbstract [
	^ self name = #RBRefactoringBrowserTest
]

{ #category : #private }
RBRefactoringBrowserTest >> executeRefactoring: aRefactoring [ 
	aRefactoring primitiveExecute.
	RBParser parseExpression: aRefactoring storeString
]

{ #category : #private }
RBRefactoringBrowserTest >> objectClassVariable [
	^Object classPool keys detect: [:each | true]
]

{ #category : #helpers }
RBRefactoringBrowserTest >> parseExpression: aString [

	^ RBParser parseExpression: aString
]

{ #category : #helpers }
RBRefactoringBrowserTest >> parseMethod: aString [ 

	^ RBParser parseMethod: aString
]

{ #category : #private }
RBRefactoringBrowserTest >> proceedThroughWarning: aBlock [ 
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]
