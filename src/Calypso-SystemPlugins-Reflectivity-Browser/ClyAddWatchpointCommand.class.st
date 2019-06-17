"
I am a command to install watchpoint into the given method or source node
"
Class {
	#name : #ClyAddWatchpointCommand,
	#superclass : #ClyMetalinkCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Watchpoints'
}

{ #category : #execution }
ClyAddWatchpointCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasWatchpoint not
]

{ #category : #activation }
ClyAddWatchpointCommand class >> contextMenuOrder [
	^3
]

{ #category : #accessing }
ClyAddWatchpointCommand >> defaultMenuIconName [
	^#haloView
]

{ #category : #accessing }
ClyAddWatchpointCommand >> defaultMenuItemName [
	^'Add watchpoint'
]

{ #category : #execution }
ClyAddWatchpointCommand >> execute [
	
	Watchpoint in: sourceNode
]
