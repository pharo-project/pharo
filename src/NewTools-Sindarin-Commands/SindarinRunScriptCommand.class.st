"
Run a given Sindarin script.
Behavior delegated to an instance of Sindarin debugger.
"
Class {
	#name : #SindarinRunScriptCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #initialization }
SindarinRunScriptCommand class >> defaultDescription [
	^'Run the current debugging script'
]

{ #category : #initialization }
SindarinRunScriptCommand class >> defaultIconName [
	^#smallDoIt
]

{ #category : #initialization }
SindarinRunScriptCommand class >> defaultName [
	^'Run script'
]

{ #category : #executing }
SindarinRunScriptCommand >> execute [
	self context executeScript
]
