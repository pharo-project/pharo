"
I am a command to install breakpoints on variables of a class
"
Class {
	#name : 'ClyDebugBreakOnVariableCommand',
	#superclass : 'ClyDebugVariableCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'accessing' }
ClyDebugBreakOnVariableCommand class >> debugPointClass [

	^ BreakDebugPoint
]
