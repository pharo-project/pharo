"
Remove the command corresponding to a given script from the list of custom extension commands for the debugger.
"
Class {
	#name : #SindarinRemoveCommandCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinRemoveCommandCommand class >> defaultDescription [
	^'Remove the command corresponding to the current debugging script'
]

{ #category : #default }
SindarinRemoveCommandCommand class >> defaultIconName [
	^#smallCancel
]

{ #category : #default }
SindarinRemoveCommandCommand class >> defaultName [
	^'Remove command'
]

{ #category : #executing }
SindarinRemoveCommandCommand >> execute [
	self context removeScriptCommand
]
