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

{ #category : 'testing' }
ClyBreakOnVariableReadCommand class >> isAbstract [

	^ false
]

{ #category : 'accessing' }
ClyBreakOnVariableReadCommand >> defaultMenuItemName [

	^ 'Break on read'
]
