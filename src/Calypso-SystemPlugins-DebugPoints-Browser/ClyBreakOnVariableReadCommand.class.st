"
I am a command to install breakpoints on readings of variables.
"
Class {
	#name : 'ClyBreakOnVariableReadCommand',
	#superclass : 'ClyDebugBreakOnVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyBreakOnVariableReadCommand class >> accessStrategy [

	^ #read
]

{ #category : 'accessing' }
ClyBreakOnVariableReadCommand >> name [

	^ 'Break on read'
]
