"
I am a command to install watchpoints on readings and writings of a variable
"
Class {
	#name : 'ClyDebugWatchOnVariableReadWriteCommand',
	#superclass : 'ClyDebugWatchOnVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyDebugWatchOnVariableReadWriteCommand class >> accessStrategy [

	^ #all
]

{ #category : 'testing' }
ClyDebugWatchOnVariableReadWriteCommand class >> isAbstract [

	^ false
]

{ #category : 'accessing' }
ClyDebugWatchOnVariableReadWriteCommand >> defaultMenuItemName [

	^ 'Watch on access'
]
