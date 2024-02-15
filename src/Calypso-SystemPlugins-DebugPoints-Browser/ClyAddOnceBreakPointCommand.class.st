"
I define a command to add a breakpoint with Once behavior on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddOnceBreakPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'testing' }
ClyAddOnceBreakPointCommand class >> canBeExecutedInContext: aBrowserContext [

	^ (super canBeExecutedInContext: aBrowserContext)
]

{ #category : 'activation' }
ClyAddOnceBreakPointCommand class >> contextMenuOrder [
	^2
]

{ #category : 'accessing' }
ClyAddOnceBreakPointCommand >> defaultMenuIconName [
	^#smallError
]

{ #category : 'accessing' }
ClyAddOnceBreakPointCommand >> defaultMenuItemName [
	^' Add Once Breakpoint to: ',  sourceNode displaySourceCode
]

{ #category : 'execution' }
ClyAddOnceBreakPointCommand >> execute [

	| dp | 
	dp := DebugPointManager
		      installNew: BreakDebugPoint
		      On: sourceNode
		      withBehaviors: { OnceBehavior }.
	self putIconInProperties: dp
]
