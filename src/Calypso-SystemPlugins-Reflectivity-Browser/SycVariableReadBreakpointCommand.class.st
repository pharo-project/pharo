"
I break on all variable reads in the selected class
"
Class {
	#name : #SycVariableReadBreakpointCommand,
	#superclass : #SycVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #execution }
SycVariableReadBreakpointCommand >> defaultMenuItemName [
	^'Break on all variable reads'
]

{ #category : #execution }
SycVariableReadBreakpointCommand >> installVariableBreakpointInTargetClass [
	VariableBreakpoint watchVariablesReadsInClass: targetClass 
]
