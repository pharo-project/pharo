"
I am a command to add conditional breakpoint into particular source node..
Condition is configured by block
 
Internal Representation and Key Implementation Points.

    Instance Variables
	conditionBlock:		<BlockClosure>
"
Class {
	#name : 'ClyAddConditionalBreakpointCommand',
	#superclass : 'ClyAddBreakpointCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Breakpoints'
}

{ #category : 'activation' }
ClyAddConditionalBreakpointCommand class >> contextMenuOrder [
	^1.2
]

{ #category : 'execution' }
ClyAddConditionalBreakpointCommand >> applyBreakpointCondition: aBreakpoint [
	aBreakpoint createCondition
]

{ #category : 'accessing' }
ClyAddConditionalBreakpointCommand >> defaultMenuItemName [
	^'Break on  ', sourceNode displaySourceCode, '  when ...'
]
