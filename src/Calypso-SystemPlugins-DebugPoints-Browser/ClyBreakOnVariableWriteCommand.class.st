"
I am a command to install breakpoints on writings of variables.
"
Class {
	#name : 'ClyBreakOnVariableWriteCommand',
	#superclass : 'ClyDebugBreakOnVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyBreakOnVariableWriteCommand class >> accessStrategy [

	^ #write
]

{ #category : 'testing' }
ClyBreakOnVariableWriteCommand class >> isAbstract [

	^ false
]

{ #category : 'accessing' }
ClyBreakOnVariableWriteCommand >> defaultMenuItemName [

	^ 'Break on write'
]
