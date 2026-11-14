"
I am a command that moves methods to another place in the code. I classify each moved method under a protocol: I ask the user for a protocol name, suggesting one for the method, using the protocol chooser of the new protocol dialog. My subclasses decide where the methods go.
"
Class {
	#name : 'SycMethodRepackagingCommand',
	#superclass : 'SycMethodRefactoringCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMethodRepackagingCommand class >> isAbstract [

	^ self = SycMethodRepackagingCommand
]

{ #category : 'categories' }
SycMethodRepackagingCommand >> classifyMethod: aMethod [

	aMethod protocol: (self newProtocolFor: aMethod)
]

{ #category : 'categories' }
SycMethodRepackagingCommand >> newProtocolFor: aMethod [

	^ StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter :dialog |
			  dialog title: 'New protocol name for: ' , aMethod name.
			  presenter
				  protocolName: (MethodClassifier suggestProtocolFor: aMethod);
				  concernedClass: aMethod methodClass ]
]
