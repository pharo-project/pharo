"
I'm a session handler used for tests purposes.
You can provide me blocks to be executed at startup or shutdown.
"
Class {
	#name : #TestSessionHandler,
	#superclass : #AbstractSessionHandler,
	#instVars : [
		'startupHandler',
		'shutdownHandler'
	],
	#category : 'System-SessionManager-Tests'
}

{ #category : #instance-creation }
TestSessionHandler class >> onShutdown: aBlockClosure [ 
	
	^ self new
		shutdownHandler: aBlockClosure;
		yourself
]

{ #category : #instance-creation }
TestSessionHandler class >> onStartup: aBlockClosure [ 
	
	^ self new
		startupHandler: aBlockClosure;
		yourself
]

{ #category : #accessing }
TestSessionHandler >> handledId [
	^ 'testSessionHandler'
]

{ #category : #printing }
TestSessionHandler >> printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title
]

{ #category : #handlers }
TestSessionHandler >> shutdown: isImageQuitting [
	shutdownHandler value
]

{ #category : #accessing }
TestSessionHandler >> shutdownHandler: aBlockClosure [
	shutdownHandler := aBlockClosure
]

{ #category : #handlers }
TestSessionHandler >> startup: isImageStarting [
	startupHandler value
]

{ #category : #accessing }
TestSessionHandler >> startupHandler: aBlockClosure [
	startupHandler := aBlockClosure
]
