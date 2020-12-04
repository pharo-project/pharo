"
Commands to install a breakpoint on the selected variable reads from the vars view in Calypso
"
Class {
	#name : #ClyBreakOnReadCommand,
	#superclass : #ClyBreakOnVariableCommand,
	#category : #'Calypso-SystemPlugins-Reflectivity-Browser-VariableBreakpoints'
}

{ #category : #execution }
ClyBreakOnReadCommand >> breakOn: variable [
	VariableBreakpoint
		watchVariableReads: variable name
		inClass: variable definingClass
]

{ #category : #execution }
ClyBreakOnReadCommand >> defaultMenuItemName [
	^'Break on read'
]
