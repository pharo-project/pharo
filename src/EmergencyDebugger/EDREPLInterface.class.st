"
I am a simple REPL interface for the ED emergency debugger.
Basically I am a readwrite stream that reads character pressed events to:
- forward them to the debugger in case it must perform special actions
- put them in myself to build a line
When I read the Character cr event, I send my contents (the line) to the debugger, then I reinitialize myself.
"
Class {
	#name : #EDREPLInterface,
	#superclass : #ReadWriteStream,
	#instVars : [
		'debugger',
		'inputBufferStream',
		'waitSemaphore'
	],
	#pools : [
		'EventSensorConstants'
	],
	#category : #'EmergencyDebugger-View'
}

{ #category : #'instance creation' }
EDREPLInterface class >> forDebugger: aSindarinDebugger [
	^ (self on: (String new: 100))
		debugger: aSindarinDebugger;
		yourself
]

{ #category : #accessing }
EDREPLInterface >> debugger: anObject [
	debugger := anObject
]

{ #category : #private }
EDREPLInterface >> inputBufferStream [

	^ inputBufferStream ifNil: [ inputBufferStream := String new writeStream ]
]

{ #category : #accessing }
EDREPLInterface >> pushChar: aChar [

	self inputBufferStream nextPut: aChar.
	self waitSemaphore signal
]

{ #category : #'command line' }
EDREPLInterface >> readEvalPrint [
	
	[ debugger isRunning ]
	whileTrue: [ 
		debugger eval: self requestLine.
		self resetREPL ].
	
	self terminateREPL
]

{ #category : #'command line' }
EDREPLInterface >> requestLine [
	| startPos char contents |
	
	startPos := position.
	[ 	
		self waitSemaphore wait.
		(char := self inputBufferStream peekLast) = Character cr ] 
	whileFalse: [ 
		(debugger performActionForChar: char)
			ifFalse: [ 
				char = Character backspace
					ifTrue: [ readLimit := position := position - 1 max: startPos ]
					ifFalse: [ self nextPut: char ].
				debugger updatePrompt: self contents ] ].
	
	contents := self contents.
	^ contents 
		copyFrom: startPos + 1 
		to: contents size
]

{ #category : #private }
EDREPLInterface >> resetREPL [

	self on: (String new: 100).
	inputBufferStream := nil
]

{ #category : #'command line' }
EDREPLInterface >> terminateREPL [

	self waitSemaphore signal.
	waitSemaphore := nil
]

{ #category : #private }
EDREPLInterface >> waitSemaphore [

	^ waitSemaphore ifNil: [ waitSemaphore := Semaphore new ]

]
