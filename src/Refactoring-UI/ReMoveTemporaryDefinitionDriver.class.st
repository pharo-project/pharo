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

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver class >> driverName [

	^ 'Move temporary definition'
]

{ #category : 'resources' }
ReMoveTemporaryDefinitionDriver >> instantiateRefactoring [ 

	^ RBMoveVariableDefinitionToSmallestValidScopeRefactoring 
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

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> sourceNode [

	^ sourceNode
]

{ #category : 'accessing' }
ReMoveTemporaryDefinitionDriver >> sourceNode: anObject [

	sourceNode := anObject
]
