"
I define a command to add a breakpoint on the AST node corresponding to the selected code
"
Class {
	#name : 'ClyAddBreakPointCommand',
	#superclass : 'ClyDebuggingPointsCommand',
	#category : 'Calypso-SystemPlugins-DebugPoints-Browser',
	#package : 'Calypso-SystemPlugins-DebugPoints-Browser'
}

{ #category : 'activation' }
ClyAddBreakPointCommand class >> contextMenuOrder [
	^1
]

{ #category : 'activation' }
ClyAddBreakPointCommand class >> methodEditorLeftBarClickActivation [

	<classAnnotation>
	^ CmdTextLeftBarDoubleClickActivation for: ClyMethodSourceCodeContext
]

{ #category : 'accessing' }
ClyAddBreakPointCommand >> defaultMenuIconName [
	^#smallDebug 
]

{ #category : 'accessing' }
ClyAddBreakPointCommand >> defaultMenuItemName [

	^ String streamContents: [ :stream | stream << 'Break on "' << sourceNode displaySourceCode << '"' ]
]

{ #category : 'execution' }
ClyAddBreakPointCommand >> execute [

	| dp |
	dp := DebugPointManager installNew: BreakDebugPoint on: sourceNode.

	self putIconInProperties: dp 
]
