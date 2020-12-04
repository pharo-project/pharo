"
I break on all variable writes in the selected class
"
Class {
	#name : #SycVariableWriteBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #activation }
SycVariableWriteBreakpointCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		order: 99
		for: ClyClass asCalypsoItemContext
]

{ #category : #execution }
SycVariableWriteBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable writes'
]

{ #category : #execution }
SycVariableWriteBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesWritesInClass: targetClass 
]
