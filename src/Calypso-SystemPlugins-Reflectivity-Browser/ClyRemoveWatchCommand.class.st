"
I am a command to remove all watchpoints from given method or source node
"
Class {
	#name : #ClyRemoveWatchCommand,
	#superclass : #ClyRemoveDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Watchpoints'
}

{ #category : #execution }
ClyRemoveWatchCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasWatch 
]

{ #category : #activation }
ClyRemoveWatchCommand class >> contextMenuOrder [
	^3
]

{ #category : #execution }
ClyRemoveWatchCommand >> debuggingToolClass [
	^Watch
]

{ #category : #'table decoration' }
ClyRemoveWatchCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #haloView
]
