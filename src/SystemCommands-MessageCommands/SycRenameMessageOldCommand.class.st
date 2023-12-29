"
Call the rename method refactoring (yes the class name is not good)
"
Class {
	#name : 'SycRenameMessageOldCommand',
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
SycRenameMessageOldCommand >> applyResultInContext: aToolContext [
	"I have problem to understand how to plug the call to the driver in the cmd architecture
	in a way that I can come back here only once the refactoring is applied."

	"to be checked"

	"aToolContext
		showMessage: originalMessage
		renamedTo: driver newMessage"
]

{ #category : 'requesting' }
SycRenameMessageOldCommand >> canAddArgs [
	^false
]

{ #category : 'requesting' }
SycRenameMessageOldCommand >> canRemoveArgs [
	^ false
]

{ #category : 'execution' }
SycRenameMessageOldCommand >> createRefactoring [
	"not used"
	^ self 
]

{ #category : 'accessing' }
SycRenameMessageOldCommand >> defaultMenuIconName [
	^#edit
]

{ #category : 'accessing' }
SycRenameMessageOldCommand >> defaultMenuItemName [
	^'Rename (old driver)'
]

{ #category : 'execution' }
SycRenameMessageOldCommand >> execute [

	driver := RERenameMethodOldDriver new
		          scopes: refactoringScopes
		          model: model
		          renameMethodSignature: originalMessage
		          in: originalMessage methodClass.
	
	driver command: self andPostAction: [ toolContext showMessage: originalMessage renamedTo: driver newMessage ].
	driver runRefactoring.
	
]

{ #category : 'execution' }
SycRenameMessageOldCommand >> isComplexRefactoring [ 

	^ false
]

{ #category : 'accessing' }
SycRenameMessageOldCommand >> newSignature [
	^ newSignature
]

{ #category : 'accessing' }
SycRenameMessageOldCommand >> newSignature: anObject [
	newSignature := anObject
]

{ #category : 'execution' }
SycRenameMessageOldCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]

{ #category : 'execution' }
SycRenameMessageOldCommand >> resultMessageSelector [
	^newSignature selector
]
