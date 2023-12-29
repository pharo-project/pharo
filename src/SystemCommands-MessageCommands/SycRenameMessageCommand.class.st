"
Call the rename method refactoring (yes the class name is not good)
"
Class {
	#name : 'SycRenameMessageCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'newSignature',
		'driver',
		'toolContext'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycRenameMessageCommand >> applyResultInContext: aToolContext [
	"I have problem to understand how to plug the call to the driver in the cmd architecture
	in a way that I can come back here only once the refactoring is applied."

	"to be checked"

	"aToolContext
		showMessage: originalMessage
		renamedTo: driver newMessage"
]

{ #category : 'requesting' }
SycRenameMessageCommand >> canAddArgs [
	^false
]

{ #category : 'requesting' }
SycRenameMessageCommand >> canRemoveArgs [
	^ false
]

{ #category : 'execution' }
SycRenameMessageCommand >> createRefactoring [
	"not used"
	^ self 
]

{ #category : 'accessing' }
SycRenameMessageCommand >> defaultMenuIconName [
	^#edit
]

{ #category : 'accessing' }
SycRenameMessageCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'execution' }
SycRenameMessageCommand >> execute [

	driver := RERenameMethodDriver new
		          scopes: refactoringScopes
		          model: model
		          renameMethodSignature: originalMessage
		          in: originalMessage methodClass.

	driver runRefactoring.
	
]

{ #category : 'execution' }
SycRenameMessageCommand >> isComplexRefactoring [ 

	^ false
]

{ #category : 'accessing' }
SycRenameMessageCommand >> newSignature [
	^ newSignature
]

{ #category : 'accessing' }
SycRenameMessageCommand >> newSignature: anObject [
	newSignature := anObject
]

{ #category : 'execution' }
SycRenameMessageCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]

{ #category : 'execution' }
SycRenameMessageCommand >> resultMessageSelector [
	^newSignature selector
]
