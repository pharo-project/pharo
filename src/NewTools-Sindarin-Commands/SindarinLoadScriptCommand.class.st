"
Loads a debugging script that has been saved in a Sindarin script class.
"
Class {
	#name : #SindarinLoadScriptCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinLoadScriptCommand class >> defaultDescription [
	^'Load a debugging script'
]

{ #category : #default }
SindarinLoadScriptCommand class >> defaultIconName [
	^#smallLoadProject
]

{ #category : #default }
SindarinLoadScriptCommand class >> defaultName [
	^'Load script'
]

{ #category : #executing }
SindarinLoadScriptCommand >> execute [
	self context loadScript
]
