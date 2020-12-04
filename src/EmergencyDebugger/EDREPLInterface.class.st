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
		'debugger'
	],
	#pools : [
		'EventSensorConstants'
	],
	#category : #'EmergencyDebugger-Core'
}

{ #category : #'instance creation' }
EDREPLInterface class >> forDebugger: aSindarinDebugger [
	^ (self on: (String new: 100))
		debugger: aSindarinDebugger;
		yourself
]

{ #category : #utilities }
EDREPLInterface class >> nextChar [
	[| evt type | 
		(Delay forMilliseconds: 20) wait.
		self sensor nextEvent
		ifNotNil: [:evtBuf | 
			type := evtBuf first.
			type = EventTypeKeyboard
				ifTrue: [evt := self currentWorld currentHand generateKeyboardEvent: evtBuf.
					evt isKeystroke
						ifTrue: [| c |
							c := evt keyCharacter.
							(c asciiValue < 128
									or: [c isSeparator
											or: [c isSpecial
													or: [c = Character backspace]]])
								ifTrue: [self sensor flushKeyboard.
									^ evt keyCharacter]]]]] repeat
]

{ #category : #accessing }
EDREPLInterface class >> sensor [
	^Sensor
]

{ #category : #accessing }
EDREPLInterface >> debugger: anObject [
	debugger := anObject
]

{ #category : #accessing }
EDREPLInterface >> nextChar [
	^ self class nextChar
]

{ #category : #'command line' }
EDREPLInterface >> readEvalPrint [
	self sensor flushKeyboard.
	[ debugger isRunning ]
		whileTrue: [ debugger eval: self requestLine.
			self resetREPL ]
]

{ #category : #'command line' }
EDREPLInterface >> requestLine [
	| startPos char contents |
	startPos := position.
	self sensor flushKeyboard.
	[ (char := self nextChar) = Character cr ]
		whileFalse: [ (debugger performActionForChar: char)
				ifFalse: [ char = Character backspace
						ifTrue: [ readLimit := position := position - 1 max: startPos ]
						ifFalse: [ self nextPut: char ].
					debugger updatePrompt: self contents ] ].
	contents := self contents.
	^ contents copyFrom: startPos + 1 to: contents size
]

{ #category : #private }
EDREPLInterface >> resetREPL [
	self on: (String new: 100)
]

{ #category : #accessing }
EDREPLInterface >> sensor [
	^self class sensor
]
