"
I am a common superclass for `InlineParameter` and `RemoveParameter` refactoring.
I have the common state and methods that are shared between the two refactorings.
"
Class {
	#name : 'RBParameterRemovalRefactoring',
	#superclass : 'RBChangeMethodNameRefactoring',
	#instVars : [
		'parameterIndex',
		'argument'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'testing' }
RBParameterRemovalRefactoring class >> isAbstract [

	^ self == RBParameterRemovalRefactoring
]

{ #category : 'validating' }
RBParameterRemovalRefactoring >> applyValidations [

	| tree |
	tree := class parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources' ].
	parameterIndex ifNil: [
			argument ifNil: [ self refactoringError: 'This method does not have an argument' ].
			parameterIndex := tree argumentNames indexOf: argument ifAbsent: [ self refactoringError: 'Select a parameter!!' ] ].
	oldSelector numArgs == 0 ifTrue: [ self refactoringError: 'This method contains no arguments' ].
	oldSelector isInfix ifTrue: [ self refactoringError: 'Cannot remove parameters of infix selectors' ]
]

{ #category : 'private' }
RBParameterRemovalRefactoring >> computeNewSelector [
	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol
]

{ #category : 'private' }
RBParameterRemovalRefactoring >> getNewSelector [
	self applyValidations.
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	^ newSelector ifNil: [ newSelector := self computeNewSelector ]
]
