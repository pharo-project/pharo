"
I am a base class for commands which repackage given methods.

I provide suitable methods for subclasses to move methods to package: 

- moveMethod: aMethod toPackage: aPackage

Subclasses should just decide what package it should be. 
"
Class {
	#name : #SycMethodRepackagingCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #testing }
SycMethodRepackagingCommand class >> isAbstract [
	^self = SycMethodRepackagingCommand
]

{ #category : #categories }
SycMethodRepackagingCommand >> classifyMethod: aMethod [

	aMethod protocol: (self newProtocolFor: aMethod)
]

{ #category : #execution }
SycMethodRepackagingCommand >> moveMethod: aMethod toPackage: aPackage [

	| existingPackage willBeExtension |
	existingPackage := aMethod package.
	existingPackage == aPackage ifTrue: [ ^ self ].

	willBeExtension := aPackage ~~ aMethod origin package.
	aPackage addMethod: aMethod.
	willBeExtension
		ifTrue: [ aMethod protocol: '*' , aPackage name ]
		ifFalse: [ self classifyMethod: aMethod ]
]

{ #category : #categories }
SycMethodRepackagingCommand >> newProtocolFor: aMethod [

	^ self asCommandActivator context requestProtocol: 'New protocol name for: ' , aMethod name suggesting: (MethodClassifier suggestProtocolFor: aMethod)
]
