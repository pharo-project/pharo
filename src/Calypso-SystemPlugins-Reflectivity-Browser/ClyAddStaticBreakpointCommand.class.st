"
I am a command to add static breakpoint into particular source node.
"
Class {
	#name : 'ClyAddStaticBreakpointCommand',
	#superclass : 'ClyAddBreakpointCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Breakpoints'
}

{ #category : 'activation' }
ClyAddStaticBreakpointCommand class >> methodEditorLeftBarClickActivation [
	<classAnnotation>

	^CmdTextLeftBarDoubleClickActivation for: ClyMethodSourceCodeContext
]

{ #category : 'execution' }
ClyAddStaticBreakpointCommand >> applyBreakpointCondition: aBreakpoint [
	aBreakpoint
]

{ #category : 'accessing' }
ClyAddStaticBreakpointCommand >> defaultMenuItemName [
	^'Break on  ', sourceNode displaySourceCode
]
