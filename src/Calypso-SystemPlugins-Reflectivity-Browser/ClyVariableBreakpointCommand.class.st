"
Commands to install a variable breakpoint on the selected variable in the source code.
If the variable is a temporary, installs the variable breakpoint on all variable nodes refering to that variable name in the method.
If the variable is an intance variable, installs the breakpoint on all methods using the variable.
"
Class {
	#name : #ClyVariableBreakpointCommand,
	#superclass : #ClyDebuggingCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #testing }
ClyVariableBreakpointCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^ self isAbstract not
		and: [ aSourceCodeContext lastSelectedSourceNode isVariable ]
]

{ #category : #activation }
ClyVariableBreakpointCommand class >> contextMenuOrder [
	^3
]

{ #category : #testing }
ClyVariableBreakpointCommand class >> isAbstract [
	^ self == ClyVariableBreakpointCommand
]

{ #category : #accessing }
ClyVariableBreakpointCommand >> defaultMenuIconName [
	^#smallDebug
]

{ #category : #execution }
ClyVariableBreakpointCommand >> execute [
	| methodOrClass |
	sourceNode isVariable
		ifFalse: [ ^ self ].
	methodOrClass := sourceNode isLocalVariable
		ifTrue: [ method ]
		ifFalse: [ method methodClass ].
	self installVariableBreakpointOn: sourceNode name in: methodOrClass
]

{ #category : #execution }
ClyVariableBreakpointCommand >> installVariableBreakpointOn: aVariableName in: methodOrClass [
	self subclassResponsibility 
]
