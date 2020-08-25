"
I am a simple logger dedicated to handle errors that could not be handled by any other tool in the system.

I log this error by:
- informing you that this error could not be handled
- using the `Smalltalk` log api to write in the _PharoDebug.log_ file

"
Class {
	#name : #DebuggerEmmergencyLogger,
	#superclass : #Object,
	#category : #'Debugger-Model-Core'
}

{ #category : #API }
DebuggerEmmergencyLogger >> debugError: anError fromSession: aDebugSession [

	self logError: anError forSession: aDebugSession
]

{ #category : #API }
DebuggerEmmergencyLogger >> logError: anError forSession: aDebugSession [

	| errDescription errStream |
	errDescription := [ anError description ]
		                  on: Error
		                  do: [ 'a ' , anError class printString ].
	errStream := WriteStream on: String new.
	errStream cr.
	errStream << ('Original error: ' , aDebugSession name asString , '.').
	errStream cr.
	errStream << ('Debugger error: ' , errDescription).
	errStream cr.

	self inform: '**Debugger opening error** ' , errStream contents.
	[ 
	Smalltalk
		logError: errStream contents
		inContext: aDebugSession interruptedContext ]
		on: Error
		do: [ :err | 
			self inform:
				'Cannot log error: ' , anError printString , 'due to error: '
				, err printString ]
]
