"
I define a command to add a watchpoint on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddWatchDebugPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'activation' }
ClyAddWatchDebugPointCommand class >> contextMenuOrder [
	^4
]

{ #category : 'accessing' }
ClyAddWatchDebugPointCommand >> defaultMenuIconName [
	^#history
]

{ #category : 'accessing' }
ClyAddWatchDebugPointCommand >> defaultMenuItemName [
	^' Add Watch to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddWatchDebugPointCommand >> execute [

	| dp | 
	dp := DebugPointManager installNew: WatchDebugPoint on: sourceNode.
	self putIconInProperties: dp
]
