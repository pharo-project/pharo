Class {
	#name : 'RBRefactoringBrowserTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'testing' }
RBRefactoringBrowserTest class >> isAbstract [
	^ self name = #RBRefactoringBrowserTest
]

{ #category : 'private' }
RBRefactoringBrowserTest >> abstractTransformationClassVariable [

	^ ReAbstractTransformation classVariables first name
]

{ #category : 'private' }
RBRefactoringBrowserTest >> executeRefactoring: aRefactoring [
	aRefactoring generateChanges.
	self parserClass parseExpression: aRefactoring storeString
]

{ #category : 'helpers' }
RBRefactoringBrowserTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : 'helpers' }
RBRefactoringBrowserTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : 'private' }
RBRefactoringBrowserTest >> parserClass [
	^ OCParser
]

{ #category : 'private' }
RBRefactoringBrowserTest >> proceedThroughWarning: aBlock [
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]
