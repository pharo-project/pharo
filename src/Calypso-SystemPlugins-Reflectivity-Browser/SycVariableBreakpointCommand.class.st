"
Commands to install a variable breakpoint on all instances variables of the selected class.
"
Class {
	#name : #SycVariableBreakpointCommand,
	#superclass : #SycSingleClassCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #activation }
SycVariableBreakpointCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		order: 99
		for: ClyClass asCalypsoItemContext
]

{ #category : #testing }
SycVariableBreakpointCommand class >> canBeExecutedInContext: aToolContext [
	^ self isAbstract not
		and: [ super canBeExecutedInContext: aToolContext ]
]

{ #category : #testing }
SycVariableBreakpointCommand class >> isAbstract [
	^self == SycVariableBreakpointCommand 
]

{ #category : #execution }
SycVariableBreakpointCommand >> defaultMenuIconName [ 
	^ #smallDebug
]

{ #category : #execution }
SycVariableBreakpointCommand >> execute [
	self installVariableBreakpointInTargetClass
]

{ #category : #execution }
SycVariableBreakpointCommand >> installVariableBreakpointInTargetClass [
	self subclassResponsibility
]
