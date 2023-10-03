"
Commands to install a breakpoint on the selected variable from the vars view in Calypso
"
Class {
	#name : 'ClyBreakOnVariableCommand',
	#superclass : 'SycVariableCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'VariableBreakpoints'
}

{ #category : 'activation' }
ClyBreakOnVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation
		byItemOf: ClyDebuggingMenuGroup
		for: ClyFullBrowserVariableContext
]

{ #category : 'testing' }
ClyBreakOnVariableCommand class >> isAbstract [
	^self == ClyBreakOnVariableCommand
]

{ #category : 'execution' }
ClyBreakOnVariableCommand >> breakOn: variable [
	self subclassResponsibility
]

{ #category : 'execution' }
ClyBreakOnVariableCommand >> defaultMenuIconName [
	^#smallDebug
]

{ #category : 'execution' }
ClyBreakOnVariableCommand >> execute [
	self breakOn: variables last
]
