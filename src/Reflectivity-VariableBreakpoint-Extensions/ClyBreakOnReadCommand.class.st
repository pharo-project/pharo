"
Commands to install a breakpoint on the selected variable reads from the vars view in Calypso
"
Class {
	#name : #ClyBreakOnReadCommand,
	#superclass : #ClyBreakOnVariableCommand,
	#category : #'Reflectivity-VariableBreakpoint-Extensions'
}

{ #category : #execution }
ClyBreakOnReadCommand >> breakOn: variable [
	VariableBreakpoint
		watchVariableReads: variable name
		in: variable definingClass
]

{ #category : #execution }
ClyBreakOnReadCommand >> defaultMenuItemName [
	^'Break on read'
]
