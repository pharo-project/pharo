"
I break on all variable writes in the selected class
"
Class {
	#name : #SycVariableWriteBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #execution }
SycVariableWriteBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable writes'
]

{ #category : #execution }
SycVariableWriteBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesWritesInClass: targetClass 
]
