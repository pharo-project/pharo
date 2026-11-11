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

{ #category : 'execution' }
ClyAddWatchDebugPointCommand >> executeRefactoring [

	| dp | 
	dp := DebugPointManager installNew: WatchDebugPoint on: self sourceNode.
	self putIconInProperties: dp
]

{ #category : 'accessing' }
ClyAddWatchDebugPointCommand >> iconName [
	^#history
]

{ #category : 'accessing' }
ClyAddWatchDebugPointCommand >> name [

	^ String streamContents: [ :stream | stream << 'Watch "' << self sourceNode displaySourceCode << '"' ]
]
