Class {
	#name : #SindarinStepToMethodEntryCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinStepToMethodEntryCommand class >> defaultDescription [
	^ 'Step to the beginning of the next method, then returns debugger control.'
]

{ #category : #initialization }
SindarinStepToMethodEntryCommand class >> defaultIconName [
	^ #glamorousInto 
]

{ #category : #default }
SindarinStepToMethodEntryCommand class >> defaultName [
	<toolbarExtensionDebugCommand: 50>
	<codeExtensionDebugCommand: 50>
	^ 'To method entry'
]

{ #category : #executing }
SindarinStepToMethodEntryCommand >> execute [ 	
	self flag: 'Not nice to always force update of debugger -> use observer?'.
	self context sindarinDebugger stepToMethodEntry.
	self context forceSessionUpdate
]
