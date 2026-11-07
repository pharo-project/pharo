"
I am a command to install breakpoints on readings and writings of variables.
"
Class {
	#name : 'ClyBreakOnVariableReadWriteCommand',
	#superclass : 'ClyDebugBreakOnVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyBreakOnVariableReadWriteCommand class >> accessStrategy [

	^ #all
]

{ #category : 'accessing' }
ClyBreakOnVariableReadWriteCommand >> name [

	^ 'Break on access'
]
