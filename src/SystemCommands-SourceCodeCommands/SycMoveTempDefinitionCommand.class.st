"
I am a command to rename temp variable in given method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : 'SycMoveTempDefinitionCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#instVars : [
		'newName'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycMoveTempDefinitionCommand >> applyCommandResult [

	super applyCommandResult.
	self context showVariableNamed: newName
]

{ #category : 'testing' }
SycMoveTempDefinitionCommand >> canBeExecuted [

	^ super canBeExecuted and: [ self context isArgOrTempVariableSelected ]
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> description [

	^ 'Moves the definition of the temporary variable closed to its user.'
]

{ #category : 'execution' }
SycMoveTempDefinitionCommand >> executeRefactoring [

	^ (ReMoveTemporaryDefinitionDriver new
		   sourceNode: self sourceNode;
		   method: self method;
		   scopes: refactoringScopes;
		   yourself) runRefactoring
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> iconName [

	^ #edit
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> name [

	^'(R) Move temp. definition'
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> order [

	^ 1.2
]
