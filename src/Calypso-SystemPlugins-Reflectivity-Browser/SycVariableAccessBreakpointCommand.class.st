"
I break on all variable accesses in the selected class
"
Class {
	#name : #SycVariableAccessBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #execution }
SycVariableAccessBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable accesses'
]

{ #category : #execution }
SycVariableAccessBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesInClass: targetClass 
]
