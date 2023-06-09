"
I am a command to launch the Remove All Senders Refactoring: removes all possible senders from a method.
"
Class {
	#name : #SycRemoveAllSendersCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #testing }
SycRemoveAllSendersCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : #execution }
SycRemoveAllSendersCommand >> createRefactoring [
	|refactoring options|
	refactoring := RBRemoveAllSendersRefactoring
		model: model
		removeSendersOf: originalMessage selector.
	options := refactoring options copy.
	options at: #inlineExpression put: [:ref :string | false].
	refactoring options: options.
	^ refactoring
]

{ #category : #accessing }
SycRemoveAllSendersCommand >> defaultMenuIconName [
	^#remove
]

{ #category : #accessing }
SycRemoveAllSendersCommand >> defaultMenuItemName [
	^'Remove all senders'
]
