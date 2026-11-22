"
I am the superclass for all menu entries in the ""Debug Points"" menu entry when right clicking in a code presenter in Calypso.

All subclasses define a command to install a debug point with or without a behavior, on an AST node
"
Class {
	#name : 'ClyDebuggingPointsCommand',
	#superclass : 'ClyDebuggingCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyDebuggingPointsCommand class >> isAbstract [
	^self == ClyDebuggingPointsCommand 
]

{ #category : 'execution' }
ClyDebuggingPointsCommand >> putIconInProperties: dp [
	dp properties at: #Icon put: self iconName.
	DebugPointManager notifyDebugPointChanged: dp.
]
