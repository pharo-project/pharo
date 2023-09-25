"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'RBRenameMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class',
		'originalMessage',
		'newMessage',
		'command',
		'postAction'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'testing' }
RBRenameMethodDriver >> canAddArgs [
	^ false
]

{ #category : 'testing' }
RBRenameMethodDriver >> canEditName [
	^ true
]

{ #category : 'testing' }
RBRenameMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : 'testing' }
RBRenameMethodDriver >> canRenameArgs [
	^ true
]

{ #category : 'To be removed' }
RBRenameMethodDriver >> command: aCommand andPostAction: aClosure [ 
	command := aCommand. "may be not needed"
	postAction := aClosure
]

{ #category : 'initialize' }
RBRenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : 'To be removed' }
RBRenameMethodDriver >> newMessage [

	^ newMessage
]

{ #category : 'execution' }
RBRenameMethodDriver >> refactoring [
	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	^ RBRenameMethodRefactoring new renameMethod: originalMessage selector in: class 
]

{ #category : 'To be removed' }
RBRenameMethodDriver >> requestNewMessage [

	| methodName dialog  |
	methodName := RBMethodName
		              selector: originalMessage selector
		              arguments: originalMessage argumentNames.
	dialog := StMethodNameEditorPresenter
		          openOn: methodName
		          withInvalidArgs: (RBCondition invalidArgumentNamesForSelector: originalMessage selector in: model)
		          canRenameArgs: self canRenameArgs
		          canRemoveArgs: self canRemoveArgs
		          canAddArgs: self canAddArgs
		          canEditName: self canEditName.
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].
	^ methodName
]

{ #category : 'execution' }
RBRenameMethodDriver >> runRefactoring [

	refactoring := self refactoring.
	[
		[
		| changes |
		newMessage := self requestNewMessage.
		refactoring newSelector: newMessage selector.
		refactoring permutation: newMessage permutation.
		refactoring renameMap: newMessage renameMap.
		refactoring newNameDoesNotRequireRefactoringPreconditions check.
		changes := refactoring generateChanges.
		self openPreviewWithChanges: changes
		"arg postAction value." ]
			on: RBApplicabilityChecksFailedError
			do: [ :err | ^ RBRefactoringError signal: err messageText ] ]
			on: RBBreakingChangeChecksFailedWarning
			do: [ :err | self furtherActionFor: (RBOverrideMethod new refactoring: refactoring).  err return ]
]

{ #category : 'initialize' }
RBRenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]
