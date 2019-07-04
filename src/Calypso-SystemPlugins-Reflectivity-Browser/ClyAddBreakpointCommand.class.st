"
I am a base class for commands which add new breakpoints into particular source node.

My subclasses should implement single method: 

- applyBreakpointCondition: aBreakpoint
"
Class {
	#name : #ClyAddBreakpointCommand,
	#superclass : #ClyMetalinkCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints'
}

{ #category : #execution }
ClyAddBreakpointCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasBreakpoint not
]

{ #category : #activation }
ClyAddBreakpointCommand class >> contextMenuOrder [
	<classAnnotationDependency>
	^1
]

{ #category : #testing }
ClyAddBreakpointCommand class >> isAbstract [
	^self = ClyAddBreakpointCommand
]

{ #category : #execution }
ClyAddBreakpointCommand >> applyBreakpointCondition: aBreakpoint [
	self subclassResponsibility 
]

{ #category : #accessing }
ClyAddBreakpointCommand >> defaultMenuIconName [
	^#smallDebug
]

{ #category : #execution }
ClyAddBreakpointCommand >> execute [
	
	| breakpoint |
	breakpoint := Breakpoint new node: sourceNode.
	self applyBreakpointCondition: breakpoint.
	breakpoint install
]
