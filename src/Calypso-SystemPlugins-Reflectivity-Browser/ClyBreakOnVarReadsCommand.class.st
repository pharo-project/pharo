"
I break on all accesses to selected target variable in the source code
"
Class {
	#name : #ClyBreakOnVarReadsCommand,
	#superclass : #ClyVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #accessing }
ClyBreakOnVarReadsCommand >> defaultMenuItemName [
	sourceNode isVariable ifFalse:[^'(unvalid node)'].
	^'Break on reads to ', sourceNode name
]

{ #category : #execution }
ClyBreakOnVarReadsCommand >> installVariableBreakpointOn: aVariableName in: methodOrClass [
	VariableBreakpoint watchVariableReads: sourceNode name inClass: methodOrClass
]
