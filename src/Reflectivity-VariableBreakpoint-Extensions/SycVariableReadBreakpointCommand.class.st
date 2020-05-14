"
I break on all variable reads in the selected class
"
Class {
	#name : #SycVariableReadBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Reflectivity-VariableBreakpoint-Extensions'
}

{ #category : #activation }
SycVariableReadBreakpointCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		order: 99
		for: ClyClass asCalypsoItemContext
]

{ #category : #execution }
SycVariableReadBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable reads'
]

{ #category : #execution }
SycVariableReadBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesReadsIn: targetClass 
]
