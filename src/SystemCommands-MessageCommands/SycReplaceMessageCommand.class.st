"
I am a command to replace senders of message selector.
"
Class {
	#name : 'SycReplaceMessageCommand',
	#superclass : 'SycMessageCmCommand',
	#instVars : [
		'newSelector',
		'selectedClass'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'execution' }
SycReplaceMessageCommand >> executeRefactoring [

	^ ((ReReplaceMessageSendDriver newApplication: self application)
		scopes: refactoringScopes
		class: selectedClass
		selector: newSelector
		oldSelector: originalMessage) runRefactoring
]

{ #category : 'accessing' }
SycReplaceMessageCommand >> icon [

	^ self iconNamed: #edit
]

{ #category : 'accessing' }
SycReplaceMessageCommand >> name [

	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : 'accessing' }
SycReplaceMessageCommand >> order [

	^ 1100
]

{ #category : 'execution' }
SycReplaceMessageCommand >> prepareFullExecution [

	| methodName dialog |
	super prepareFullExecution.
	selectedClass := originalMessage methodClass.
	methodName := RBMethodName selector: originalMessage selector arguments: originalMessage argumentNames copy.
	dialog := (StMethodNameEditorPresenter newApplication: self application model: methodName) openModal.
	dialog cancelled ifTrue: [ CmCommandAborted signal ].
	originalMessage selector = methodName selector ifTrue: [ CmCommandAborted signal ].
	newSelector := methodName
]

{ #category : 'factory method' }
SycReplaceMessageCommand >> refactoringClass [

	^ ReReplaceMessageSendTransformation
]

{ #category : 'execution' }
SycReplaceMessageCommand >> resultMessageSelector [

	^ newSelector selector
]
