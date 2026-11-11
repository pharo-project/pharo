"
I am a command to add new argument into the message.

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentDefaultValue:		<Object>
	newSelector:		<Symbol>
"
Class {
	#name : 'SycAddMessageArgumentCommand',
	#superclass : 'SycMessageCmCommand',
	#instVars : [
		'newSelector'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycAddMessageArgumentCommand class >> canBeExecutedInContext: aToolContext [

	"This method overrides the superclass definition to make appear this option only for methods.

	Using the refactor from the method selection panel works fine.
	Otherwise createValidNameArgument creates an argument name that is not already in use (for example: if anObject is used then it return anObject1).
	However when we ask this refactoring from a message we don't know the method and cannot do those checks. (If there is only one)"
	^aToolContext isMethodSelected
]

{ #category : 'execution' }
SycAddMessageArgumentCommand >> applyCommandResult [
	"The refactoring was applied by the driver while executing the command."

	
]

{ #category : 'testing' }
SycAddMessageArgumentCommand >> canEditName [
	^ false
]

{ #category : 'requesting' }
SycAddMessageArgumentCommand >> canRemoveArgs [
	^ false
]

{ #category : 'execution' }
SycAddMessageArgumentCommand >> createRefactoring [
	"not used"

]

{ #category : 'execution' }
SycAddMessageArgumentCommand >> executeRefactoring [

	ReAddParameterDriver new
		scopes: refactoringScopes
		model: model
		addParameterToMethodSignature: originalMessage
		in: originalMessage methodClass;
		runRefactoring
]

{ #category : 'accessing' }
SycAddMessageArgumentCommand >> icon [

	^ self iconNamed: #smallAdd
]

{ #category : 'accessing' }
SycAddMessageArgumentCommand >> name [

	^ 'Add argument'
]

{ #category : 'accessing' }
SycAddMessageArgumentCommand >> newSelector [
	^ newSelector
]

{ #category : 'accessing' }
SycAddMessageArgumentCommand >> newSelector: anObject [
	newSelector := anObject
]

{ #category : 'accessing' }
SycAddMessageArgumentCommand >> order [

	^ 30
]

{ #category : 'execution' }
SycAddMessageArgumentCommand >> resultMessageSelector [
	^newSelector
]
