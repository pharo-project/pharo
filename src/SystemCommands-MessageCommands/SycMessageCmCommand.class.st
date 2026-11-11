"
I am a temporary Commander2 bridge for message/signature commands. I subclass the legacy message command to keep its dialogs and refactoring behavior, and add the Commander2 execution lifecycle on top. I will be removed when Commander v1 is gone.
"
Class {
	#name : 'SycMessageCmCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'accessing' }
SycMessageCmCommand >> orderInContext: aToolContext [

	^ aToolContext isMethodSourceContext
		ifTrue: [ 1.3 ]
		ifFalse: [ self order ]
]
