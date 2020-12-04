"
I break on all accesses to selected target variable in the source code
"
Class {
	#name : #ClyBreakOnVarAccessCommand,
	#superclass : #ClyVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #testing }
ClyBreakOnVarAccessCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^ (super canBeExecutedInContext: aSourceCodeContext)
		and: [ aSourceCodeContext selectedSourceNode isArgumentVariable not ]
]

{ #category : #accessing }
ClyBreakOnVarAccessCommand >> defaultMenuItemName [
	sourceNode isVariable ifFalse:[^'(unvalid node)'].
	^'Break on accesses to ', sourceNode name
]

{ #category : #execution }
ClyBreakOnVarAccessCommand >> installVariableBreakpointOn: aVariableName in: methodOrClass [
	VariableBreakpoint watchVariable: sourceNode name inClass: methodOrClass
]
