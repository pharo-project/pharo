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
		'newName',
		'toolContext'
	],
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycMoveTempDefinitionCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isArgOrTempVariableSelected ]
]

{ #category : 'execution' }
SycMoveTempDefinitionCommand >> applyResultInContext: aSourceCodeContext [

	super applyResultInContext: aSourceCodeContext.
	aSourceCodeContext showVariableNamed: newName
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> defaultMenuIconName [

	^ #edit
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> defaultMenuItemName [

	^'(R) Move temp. definition'
]

{ #category : 'accessing' }
SycMoveTempDefinitionCommand >> description [

	^ 'Moves the definition of the temporary variable closed to its user.'
]

{ #category : 'execution' }
SycMoveTempDefinitionCommand >> execute [

	^ (ReMoveTemporaryDefinitionDriver new
		   sourceNode: sourceNode;
		   method: method;
		   scopes: toolContext refactoringScopes;
		   yourself) runRefactoring
]

{ #category : 'execution' }
SycMoveTempDefinitionCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]
