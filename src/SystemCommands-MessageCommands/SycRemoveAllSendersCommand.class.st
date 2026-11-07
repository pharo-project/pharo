"
I am a command to launch the Remove All Senders Refactoring: removes all possible senders from a method.
"
Class {
	#name : 'SycRemoveAllSendersCommand',
	#superclass : 'SycMessageCmCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycRemoveAllSendersCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : 'execution' }
SycRemoveAllSendersCommand >> createRefactoring [
	|refactoring options|
	refactoring := RBRemoveAllSendersRefactoring
		model: model
		removeSendersOf: originalMessage selector.
	options := refactoring options copy.
	options at: #shouldNotCreateExtraBindings put: [:ref :string | false].
	refactoring options: options.
	^ refactoring
]

{ #category : 'accessing' }
SycRemoveAllSendersCommand >> icon [

	^ self iconNamed: #remove
]

{ #category : 'accessing' }
SycRemoveAllSendersCommand >> name [

	^ 'Remove all senders'
]

{ #category : 'accessing' }
SycRemoveAllSendersCommand >> order [

	^ 35
]
