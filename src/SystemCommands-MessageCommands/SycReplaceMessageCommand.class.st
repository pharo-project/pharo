"
I am a command to replace senders of message selector.
"
Class {
	#name : #SycReplaceMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSelector',
		'selectedClass'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #execution }
SycReplaceMessageCommand >> createRefactoring [
	self flag: 'TODO'."Improve preview to add args when change the sender with empty args"

	^ newSelector newArgs
		ifNotEmpty:[
		(self refactoringClass
		model: model
		replaceMethod: originalMessage selector
		in: selectedClass
		to: newSelector selector
		permutation: newSelector permutation
		inAllClasses: self replaceInAllClasses)
		initializers: newSelector newArgs values ]
	ifEmpty:[
	 (self refactoringClass
		model: model
		replaceMethod: originalMessage selector
		in: selectedClass
		to: newSelector selector
		permutation: newSelector permutation
		inAllClasses: self replaceInAllClasses)
		initializers: newSelector newArgs ]
]

{ #category : #accessing }
SycReplaceMessageCommand >> defaultMenuIconName [
	^#edit
]

{ #category : #accessing }
SycReplaceMessageCommand >> defaultMenuItemName [
	"Make sure that the user knows that this is a transformation by adding (T) in front of the menu item name."

	^ self refactoringClass menuItemString
]

{ #category : #execution }
SycReplaceMessageCommand >> prepareFullExecutionInContext: aToolContext [

	| methodName dialog |
	super prepareFullExecutionInContext: aToolContext.

	"This is for the code editor
		selectedClass := aToolContext selectedMethod methodClass.

	The following expression 	selectedClass := originalMessage methodClass. is for the method list. The context is pain bad because it cannot handle both. We kept the method list.
	Check class side canBeExecutedInContext: if you know how to fix it"

	selectedClass := originalMessage methodClass.
	methodName := RBMethodName
		              selector: originalMessage selector
		              arguments: originalMessage argumentNames copy.
	dialog := SycMethodNameEditorPresenter openOn: methodName.
	dialog cancelled ifTrue: [ CmdCommandAborted signal ].

	originalMessage selector = methodName selector ifTrue: [
		CmdCommandAborted signal ].
	newSelector := methodName
]

{ #category : #'factory method' }
SycReplaceMessageCommand >> refactoringClass [

	^ RBReplaceMessageSendTransformation
]

{ #category : #execution }
SycReplaceMessageCommand >> replaceInAllClasses [
	^ self confirm: 'Do you want replace senders of method in the whole classes?'
]
