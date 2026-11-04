"
Call the rename method refactoring (yes the class name is not good)
"
Class {
	#name : 'SycRenameMessageCommand',
	#superclass : 'SycChangeMessageMenuCmCommand',
	#instVars : [
		'newSignature',
		'driver'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycRenameMessageCommand >> applyCommandResult [
	"I have problem to understand how to plug the call to the driver in the cmd architecture
	in a way that I can come back here only once the refactoring is applied."

	"to be checked"

	"aToolContext
		showMessage: originalMessage
		renamedTo: driver newMessage" 
]

{ #category : 'requesting' }
SycRenameMessageCommand >> canAddArgs [
	^ false
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
SycRenameMessageCommand >> description [

	^ 'Rename method'
]

{ #category : 'execution' }
SycRenameMessageCommand >> executeRefactoring [

	driver := ReRenameMethodDriver new
		          scopes: refactoringScopes
		          model: model
		          renameMethodSignature: originalMessage
		          in: originalMessage methodClass.
	driver runRefactoring.
	
]

{ #category : 'accessing' }
SycRenameMessageCommand >> iconName [
	^ #edit
]

{ #category : 'accessing' }
SycRenameMessageCommand >> name [
	^ '(R) Rename'
]

{ #category : 'accessing' }
SycRenameMessageCommand >> newSignature [

	^ newSignature
]

{ #category : 'accessing' }
SycRenameMessageCommand >> newSignature: anObject [

	newSignature := anObject
]

{ #category : 'accessing' }
SycRenameMessageCommand >> order [

	^ 1.7
]

{ #category : 'accessing' }
SycRenameMessageCommand >> orderInContext: aToolContext [

	^ aToolContext isMethodSourceContext
		ifTrue: [ 1.2 ]
		ifFalse: [ self order ]
]

{ #category : 'execution' }
SycRenameMessageCommand >> resultMessageSelector [

	^ newSignature selector
]
