"
Call the rename method refactoring (yes the class name is not good)
"
Class {
	#name : 'SycRenameMessage2Command',
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
SycRenameMessage2Command >> applyResultInContext: aToolContext [
	"I have problem to understand how to plug the call to the driver in the cmd architecture
	in a way that I can come back here only once the refactoring is applied."

	"to be checked"

	"aToolContext
		showMessage: originalMessage
		renamedTo: driver newMessage"
]

{ #category : 'requesting' }
SycRenameMessage2Command >> canAddArgs [
	^false
]

{ #category : 'requesting' }
SycRenameMessage2Command >> canRemoveArgs [
	^ false
]

{ #category : 'execution' }
SycRenameMessage2Command >> createRefactoring [
	"not used"
	^ self 
]

{ #category : 'accessing' }
SycRenameMessage2Command >> defaultMenuIconName [
	^#edit
]

{ #category : 'accessing' }
SycRenameMessage2Command >> defaultMenuItemName [
	^'Rename'
]

{ #category : 'execution' }
SycRenameMessage2Command >> execute [

	driver := RBRenameMethodDriver new
		          scopes: refactoringScopes
		          model: model
		          renameMethodSignature: originalMessage
		          in: refactoredClass.
	driver command: self andPostAction: [ toolContext showMessage: originalMessage renamedTo: driver newMessage ].
	driver runRefactoring.
	
]

{ #category : 'execution' }
SycRenameMessage2Command >> isComplexRefactoring [ 

	^ false
]

{ #category : 'accessing' }
SycRenameMessage2Command >> newSignature [
	^ newSignature
]

{ #category : 'accessing' }
SycRenameMessage2Command >> newSignature: anObject [
	newSignature := anObject
]

{ #category : 'execution' }
SycRenameMessage2Command >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext
]

{ #category : 'execution' }
SycRenameMessage2Command >> resultMessageSelector [
	^newSignature selector
]
