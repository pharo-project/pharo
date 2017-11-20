"
RBMoveInstVarToClassRefactoring knows how to move instance variable from one class to another.

Instance Variables
	newClass:		<RBClass>

newClass
	- class, in which user moves an instance variable
"
Class {
	#name : #RBMoveInstVarToClassRefactoring,
	#superclass : #RBVariableRefactoring,
	#instVars : [
		'oldClass'
	],
	#category : #Refactoring-Core-Refactorings
}

{ #category : #'as yet unclassified' }
RBMoveInstVarToClassRefactoring class >> model: aRBSmalltalk variable: aVarName class: aClass oldClass: anOldClass [
	^(self new)
		model: aRBSmalltalk;
		variable: aVarName class: aClass;
		oldClass: anOldClass;
		yourself.
]

{ #category : #transforming }
RBMoveInstVarToClassRefactoring >> collectSendersOfInstVar [
	| brokenSenders subclasses hasInstVar |
	hasInstVar := false.
	brokenSenders := OrderedCollection new.
	oldClass superclass instanceVariableNames
		do: [ :each | 
			each = variableName
				ifTrue: [ hasInstVar := true ] ].
	hasInstVar
		ifTrue: [ ^ brokenSenders ]
		ifFalse: [ 
			brokenSenders := (oldClass selectors
				select: [ :each | (oldClass methodFor: each) source includesSubstring: variableName asString ]
				thenCollect: [ :each | oldClass methodFor: each ]) asOrderedCollection.
			subclasses := oldClass subclasses.
			subclasses
				do: [ :subclass | 
					subclass selectors
						do: [ :each | 
							((subclass methodFor: each) source includesSubstring: ' ' , variableName asString , ' ')
								ifTrue: [ brokenSenders add: (subclass methodFor: each) ] ] ].
			^ brokenSenders ]
]

{ #category : #accessing }
RBMoveInstVarToClassRefactoring >> oldClass: aClass [
	oldClass := self classObjectFor: aClass
]

{ #category : #preconditions }
RBMoveInstVarToClassRefactoring >> preconditions [
	^ RBCondition
		withBlock: [ 
			(class hierarchyDefinesInstanceVariable: variableName)
				ifTrue: [ self refactoringError: 'Already defined in hierarchy' ].true ]
]

{ #category : #transforming }
RBMoveInstVarToClassRefactoring >> transform [
	| brokenSenders |
	class addInstanceVariable: variableName asString.
	oldClass removeInstanceVariable: variableName asString.
	brokenSenders := self collectSendersOfInstVar.
	brokenSenders ifNotEmpty: [ 
		Smalltalk tools messageList
			browse: (brokenSenders collect:#method)
			title: 'Broken methods' 
			autoSelect: brokenSenders first selector] 
]
