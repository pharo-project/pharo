"
I am a command to add once breakpoint into particular source node.
"
Class {
	#name : 'ClyAddOnceBreakpointCommand',
	#superclass : 'ClyAddBreakpointCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Breakpoints'
}

{ #category : 'execution' }
ClyAddOnceBreakpointCommand >> applyBreakpointCondition: aBreakpoint [
	aBreakpoint once: true
]

{ #category : 'accessing' }
ClyAddOnceBreakpointCommand >> defaultMenuItemName [
	^'Break on  ', sourceNode displaySourceCode, '  once'
]
