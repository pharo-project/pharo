"
I define a command to add a breakpoint with Count behavior on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddCountDebugPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyAddCountDebugPointCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext)
]

{ #category : 'activation' }
ClyAddCountDebugPointCommand class >> contextMenuOrder [
	^4
]

{ #category : 'accessing' }
ClyAddCountDebugPointCommand >> defaultMenuIconName [
	^#smallDebug
]

{ #category : 'accessing' }
ClyAddCountDebugPointCommand >> defaultMenuItemName [
	^' Add Counter to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddCountDebugPointCommand >> execute [

	| dp | 
	dp := DebugPointManager
		      installNew: DebugPoint
		      on: sourceNode
		      withBehaviors: { CountBehavior }.

	dp name: #Counter.
	self putIconInProperties: dp
]
