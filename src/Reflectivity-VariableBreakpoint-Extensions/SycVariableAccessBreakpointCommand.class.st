"
I break on all variable accesses in the selected class
"
Class {
	#name : #SycVariableAccessBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Reflectivity-VariableBreakpoint-Extensions'
}

{ #category : #activation }
SycVariableAccessBreakpointCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		order: 99
		for: ClyClass asCalypsoItemContext
]

{ #category : #execution }
SycVariableAccessBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable accesses'
]

{ #category : #execution }
SycVariableAccessBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesIn: targetClass 
]
