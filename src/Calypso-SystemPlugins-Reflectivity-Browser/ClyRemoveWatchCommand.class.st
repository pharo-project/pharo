"
I am a command to remove all watchpoints from given method or source node
"
Class {
	#name : 'ClyRemoveWatchCommand',
	#superclass : 'ClyRemoveDebuggingCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Watchpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Watchpoints'
}

{ #category : 'testing' }
ClyRemoveWatchCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasWatch
]

{ #category : 'activation' }
ClyRemoveWatchCommand class >> contextMenuOrder [
	^3
]

{ #category : 'activation' }
ClyRemoveWatchCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycAdvancedDebuggingMenuActivation
			byItemOf: ClyAdvancedDebuggingMenuGroup order: self contextMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
ClyRemoveWatchCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycAdvancedDebuggingMenuActivation
		byItemOf: ClyAdvancedDebuggingMenuGroup order: self contextMenuOrder for: ClyMethodSourceCodeContext
]

{ #category : 'execution' }
ClyRemoveWatchCommand >> debuggingToolClass [
	^Watch
]

{ #category : 'table decoration' }
ClyRemoveWatchCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #haloView
]
