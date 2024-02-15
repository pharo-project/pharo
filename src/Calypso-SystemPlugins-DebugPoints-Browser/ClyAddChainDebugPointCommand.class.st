"
I define a command to add a breakpoint with ChainLink behavior on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddChainDebugPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyAddChainDebugPointCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext)
]

{ #category : 'activation' }
ClyAddChainDebugPointCommand class >> contextMenuOrder [
	^4
]

{ #category : 'accessing' }
ClyAddChainDebugPointCommand >> defaultMenuIconName [
	^#smallDebug
]

{ #category : 'accessing' }
ClyAddChainDebugPointCommand >> defaultMenuItemName [
	^' Add Chain Link to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddChainDebugPointCommand >> execute [

	| dp |
	dp := (DebugPointManager
		       installNew: DebugPoint
		       On: sourceNode
		       withBehaviors: { ChainBehavior }) name: #ChainLink.

	self putIconInProperties: dp 
]
