"
I am a command to remove all breakpoints from given method or source node
"
Class {
	#name : #ClyRemoveBreakpointCommand,
	#superclass : #ClyRemoveDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints'
}

{ #category : #execution }
ClyRemoveBreakpointCommand class >> canBeExecutedInContext: aBrowserContext [

	^aBrowserContext isSelectedItemHasBreakpoint 
]

{ #category : #activation }
ClyRemoveBreakpointCommand class >> contextMenuOrder [
	^1
]

{ #category : #execution }
ClyRemoveBreakpointCommand >> debuggingToolClass [
	^Breakpoint
]

{ #category : #'table decoration' }
ClyRemoveBreakpointCommand >> tableCellIcon [
	^Smalltalk ui iconNamed: #halt
]
