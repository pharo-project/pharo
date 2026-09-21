"
Call the rename method refactoring 
"
Class {
	#name : 'SycRenameMethodCommand2',
	#superclass : 'SycChangeMessageSignatureCommand',
	#instVars : [
		'newSignature',
		'driver',
		'toolContext'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycRenameMethodCommand2 >> applyResultInContext: aToolContext [
	"I have problem to understand how to plug the call to the driver in the cmd architecture
	in a way that I can come back here only once the refactoring is applied."

	"to be checked"

	"aToolContext
		showMessage: originalMessage
		renamedTo: driver newMessage" 
]

{ #category : 'requesting' }
SycRenameMethodCommand2 >> canAddArgs [
	^ false
]

{ #category : 'requesting' }
SycRenameMethodCommand2 >> canRemoveArgs [
	^ false
]

{ #category : 'execution' }
SycRenameMethodCommand2 >> createRefactoring [
	"not used"
	^ self
]

{ #category : 'accessing' }
SycRenameMethodCommand2 >> defaultMenuIconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameMethodCommand2 >> defaultMenuItemName [
	^ '(R) Rename method'
]

{ #category : 'accessing' }
SycRenameMethodCommand2 >> description [

	^ 'Rename method'
]

{ #category : 'execution' }
SycRenameMethodCommand2 >> execute [

	driver := ReRenameMethodDriver2 new
		          scopes: refactoringScopes
		          model: model
		          renameMethodSignature: originalMessage
		          in: originalMessage methodClass.
	driver runRefactoring.
	
]

{ #category : 'accessing' }
SycRenameMethodCommand2 >> newSignature [

	^ newSignature
]

{ #category : 'accessing' }
SycRenameMethodCommand2 >> newSignature: anObject [

	newSignature := anObject
]

{ #category : 'execution' }
SycRenameMethodCommand2 >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]

{ #category : 'execution' }
SycRenameMethodCommand2 >> resultMessageSelector [

	^ newSignature selector
]
