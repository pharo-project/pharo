"
I am a simple logger dedicated to handle errors that could not be handled by any other tool in the system.

I log this error by:
- informing you that this error could not be handled
- using the `Smalltalk` log api to write in the _PharoDebug.log_ file

"
Class {
	#name : 'DebuggerEmergencyLogger',
	#superclass : 'Object',
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'API' }
DebuggerEmergencyLogger >> debugError: anError fromSession: aDebugSession [

	self logError: anError forSession: aDebugSession
]

{ #category : 'API' }
DebuggerEmergencyLogger >> logError: anError forSession: aDebugSession [

	| errDescription |
	errDescription := [ anError description ]
		                  on: Error
		                  do: [ 'a ' , anError class printString ].

	[
	Smalltalk
		logError: errDescription contents
		inContext: aDebugSession interruptedContext ]
		on: Error
		do: [ :err |
			InformativeNotification signal: (String streamContents: [ :str |
					 str << 'Cannot log error:'.
					 str space.
					 str << anError description.
					 str space.
					 str << 'due to error:'.
					 str space.
					 str << err description ]) ]
]
