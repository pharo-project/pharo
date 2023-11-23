"
I am a command to install watchpoint into the given method or source node
"
Class {
	#name : 'ClyAddWatchCommand',
	#superclass : 'ClyAdvancedDebuggingCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Watchpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Watchpoints'
}

{ #category : 'testing' }
ClyAddWatchCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext) and: [
		  aBrowserContext isSelectedItemHasWatch not ]
]

{ #category : 'activation' }
ClyAddWatchCommand class >> contextMenuOrder [
	^3
]

{ #category : 'accessing' }
ClyAddWatchCommand >> defaultMenuIconName [
	^#haloView
]

{ #category : 'accessing' }
ClyAddWatchCommand >> defaultMenuItemName [
	^' Add Watchpoint to  ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddWatchCommand >> execute [

	Watch in: sourceNode
]
