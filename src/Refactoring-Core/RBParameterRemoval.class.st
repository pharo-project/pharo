"
I am a common superclass for `InlineParameter` and `RemoveParameter` refactoring.
I have the common state and methods that are shared between the two refactorings.
"
Class {
	#name : 'RBParameterRemoval',
	#superclass : 'RBChangeMethodNameRefactoring',
	#instVars : [
		'parameterIndex',
		'argument'
	],
	#category : 'Refactoring-Core-Refactorings-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings-Unused'
}

{ #category : 'testing' }
RBParameterRemoval class >> isAbstract [

	^ self == RBParameterRemoval
]

{ #category : 'validating' }
RBParameterRemoval >> applyValidations [

	| tree |
	(class directlyDefinesMethod: oldSelector) ifFalse: [
		self refactoringError: 'Method doesn''t exist' ].
	tree := class parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources' ].
	argument ifNil: [
		self refactoringError: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames
		                  indexOf: argument
		                  ifAbsent: [
		                  self refactoringError: 'Select a parameter!!' ].
	oldSelector numArgs == 0 ifTrue: [
		self refactoringError: 'This method contains no arguments' ].
	oldSelector isInfix ifTrue: [
		self refactoringError:
			'Cannot remove parameters of infix selectors' ]
]

{ #category : 'private' }
RBParameterRemoval >> computeNewSelector [
	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol
]

{ #category : 'private' }
RBParameterRemoval >> getNewSelector [
	self applyValidations.
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	^ newSelector ifNil: [ newSelector := self computeNewSelector ]
]
