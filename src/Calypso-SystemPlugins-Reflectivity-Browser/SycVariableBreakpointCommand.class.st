"
Commands to install a variable breakpoint on all instances variables of the selected class.
"
Class {
	#name : 'SycVariableBreakpointCommand',
	#superclass : 'SycSingleClassCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'VariableBreakpoints'
}

{ #category : 'activation' }
SycVariableBreakpointCommand class >> browserContextMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		order: 99
		for: ClyClass asCalypsoItemContext
]

{ #category : 'testing' }
SycVariableBreakpointCommand class >> isAbstract [
	^self == SycVariableBreakpointCommand
]

{ #category : 'accessing' }
SycVariableBreakpointCommand >> defaultMenuIconName [
	^ #halt
]

{ #category : 'execution' }
SycVariableBreakpointCommand >> execute [
	self installVariableBreakpointInTargetClass
]

{ #category : 'execution' }
SycVariableBreakpointCommand >> installVariableBreakpointInTargetClass [
	self subclassResponsibility
]
