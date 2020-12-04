"
Creates an anonymous command class with a debugging script. 
The created command class can be instantiated by the debugger as a custom debugging action.
The created command class has a pragma that identifies itself as a custom debugging action.
"
Class {
	#name : #SindarinCreateCommandFromScriptCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinCreateCommandFromScriptCommand class >> defaultDescription [
	^'Create a command from the current debugging script'
]

{ #category : #initialization }
SindarinCreateCommandFromScriptCommand class >> defaultIconName [
	^#smallAdd 
]

{ #category : #default }
SindarinCreateCommandFromScriptCommand class >> defaultName [
	^'Create new command from script'
]

{ #category : #executing }
SindarinCreateCommandFromScriptCommand >> execute [
	self context createCommandFromScript
]
