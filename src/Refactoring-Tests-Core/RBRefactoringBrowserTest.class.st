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
RBRefactoringBrowserTest >> convertInterval: anInterval for: aString [ 
	"Convert the interval to ignore differences in end of line conventions."

	^anInterval
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

{ #category : #private }
RBRefactoringBrowserTest >> proceedThroughWarning: aBlock [ 
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]
