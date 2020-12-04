"
I break on all accesses to selected target variable in the source code
"
Class {
	#name : #ClyBreakOnVarWritesCommand,
	#superclass : #ClyVariableBreakpointCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #testing }
ClyBreakOnVarWritesCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^ (super canBeExecutedInContext: aSourceCodeContext)
		and: [ aSourceCodeContext selectedSourceNode isArgumentVariable not ]
]

{ #category : #accessing }
ClyBreakOnVarWritesCommand >> defaultMenuItemName [
	sourceNode isVariable ifFalse:[^'(unvalid node)'].
	^'Break on writes to ', sourceNode name
]

{ #category : #execution }
ClyBreakOnVarWritesCommand >> installVariableBreakpointOn: aVariableName in: methodOrClass [
	VariableBreakpoint watchVariableWrites: sourceNode name inClass: methodOrClass
]
