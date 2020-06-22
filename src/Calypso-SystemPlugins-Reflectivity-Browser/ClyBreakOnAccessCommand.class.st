"
Commands to install a breakpoint on the selected variable accesses from the vars view in Calypso
"
Class {
	#name : #ClyBreakOnAccessCommand,
	#superclass : #ClyBreakOnVariableCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #execution }
ClyBreakOnAccessCommand >> breakOn: variable [
	VariableBreakpoint
		watchVariable: variable name
		inClass: variable definingClass
]

{ #category : #execution }
ClyBreakOnAccessCommand >> defaultMenuItemName [
	^'Break on access'
]
