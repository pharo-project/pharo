"
I define a command group that should contain all debug point commands in calypso
"
Class {
	#name : 'ClyDebuggingPointsMenuGroup',
	#superclass : 'CmdMenuGroup',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyDebuggingPointsMenuGroup >> isInlined [

	^ true
]

{ #category : 'accessing' }
ClyDebuggingPointsMenuGroup >> order [
	^0.9
]
