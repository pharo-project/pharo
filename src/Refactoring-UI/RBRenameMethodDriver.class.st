Class {
	#name : #RBRenameMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'selector',
		'originalMessage',
		'refactoring',
		'newMessage',
		'command',
		'postAction'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #testing }
RBRenameMethodDriver >> canAddArgs [
	^ false
]

{ #category : #testing }
RBRenameMethodDriver >> canEditName [
	^ true
]

{ #category : #testing }
RBRenameMethodDriver >> canRemoveArgs [
	^ false
]

{ #category : #testing }
RBRenameMethodDriver >> canRenameArgs [
	^ true
]

{ #category : #'as yet unclassified' }
RBRenameMethodDriver >> command: aCommand andPostAction: aClosure [ 
	command := aCommand. "may be not needed"
	postAction := aClosure
]

{ #category : #execution }
RBRenameMethodDriver >> execute [

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
			do: [ :err | self furtherActionFor: (RBInstanceVariableStillReferenced new refactoring: refactoring).  err return ]
]

{ #category : #initialize }
RBRenameMethodDriver >> model: aModel renameMethodSignature: aMessage in: aClass [ 
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]

{ #category : #'as yet unclassified' }
RBRenameMethodDriver >> newMessage [

	^ newMessage
]

{ #category : #execution }
RBRenameMethodDriver >> refactoring [
	"here we do not fully configure the refactoring because we are missing information such as the signature.
	Since we want to let the refactoring validate input we need to create it upfront then configure later."
	
	^ RBRenameMethodRefactoring new renameMethod: originalMessage selector in: class 
]

{ #category : #'as yet unclassified' }
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

{ #category : #initialize }
RBRenameMethodDriver >> scopes: refactoringScope model: aModel renameMethodSignature: aMessage in: aClass [

	scopes := refactoringScope.
	model := aModel.
	originalMessage := aMessage.
	"while we could think that the class is not needed, it is because a rename should not override existing methods."
	class := aClass
]
