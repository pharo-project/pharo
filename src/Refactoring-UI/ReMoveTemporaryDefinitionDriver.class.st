"
I am a driver to manage the interractions during a move temporary variable definition closer to its users.

I'll check that the interval selected by the user is right. 
"
Class {
	#name : 'ReMoveTemporaryDefinitionDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'sourceNode',
		'method'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
ReMoveTemporaryDefinitionDriver >> configureRefactoring [

	refactoring := RBMoveVariableDefinitionToSmallestValidScopeRefactoring 
		bindTight: sourceNode sourceInterval 
		in: method origin 
		selector: method selector

]

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> method [

	^ method
]

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> method: anObject [

	method := anObject
]

{ #category : 'execution' }
ReMoveTemporaryDefinitionDriver >> runRefactoring [

	self configureRefactoring.
	refactoring prepareForExecution. 
	"This is needed to set some info in the refactoring such as the parse tree or the defining node."
	
	[ refactoring eagerlyCheckApplicabilityPreconditions ] 
		on: RBRefactoringError 
		do: [ :ex| self application inform: 'The temp cannot be move closer'. ^ self ].
	self applyChanges
]

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> sourceNode [

	^ sourceNode
]

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> sourceNode: anObject [

	sourceNode := anObject
]
