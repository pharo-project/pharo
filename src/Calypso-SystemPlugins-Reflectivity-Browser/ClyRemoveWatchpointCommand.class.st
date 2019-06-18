"
I am a command to remove all watchpoints from given method or source node
"
Class {
	#name : #ClyRemoveWatchpointCommand,
	#superclass : #ClyRemoveMetalinkCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Watchpoints'
}

{ #category : #execution }
ClyRemoveWatchpointCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasWatchpoint 
]

{ #category : #activation }
ClyRemoveWatchpointCommand class >> contextMenuOrder [
	^3
]

{ #category : #execution }
ClyRemoveWatchpointCommand >> metalinkManagerClass [
	^Watchpoint
]

{ #category : #'table decoration' }
ClyRemoveWatchpointCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #haloView
]
