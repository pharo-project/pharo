"
I am a base class for commands which repackage given methods.

I provide suitable methods for subclasses to move methods to package: 

- moveMethod: aMethod toPackage: aPackage

Subclasses should just decide what package it should be. 
"
Class {
	#name : 'SycMethodRepackagingCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'testing' }
SycMethodRepackagingCommand class >> isAbstract [
	^self = SycMethodRepackagingCommand
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
