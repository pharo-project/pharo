"
Commands to install a breakpoint on the selected variable writes from the vars view in Calypso
"
Class {
	#name : #ClyBreakOnWriteCommand,
	#superclass : #ClyBreakOnVariableCommand,
	#category : #'Reflectivity-VariableBreakpoint-Extensions'
}

{ #category : #execution }
ClyBreakOnWriteCommand >> breakOn: variable [
	VariableBreakpoint
		watchVariableWrites: variable name
		in: variable definingClass
]

{ #category : #execution }
ClyBreakOnWriteCommand >> defaultMenuItemName [
	^'Break on write'
]
