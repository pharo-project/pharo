"
Saves a given debugging script to a Sindarin script class.
"
Class {
	#name : #SindarinSaveScriptCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinSaveScriptCommand class >> defaultDescription [
	^'Save the current debugging script'
]

{ #category : #initialization }
SindarinSaveScriptCommand class >> defaultIconName [
	^#smallSave
]

{ #category : #default }
SindarinSaveScriptCommand class >> defaultName [
	^'Save script'
]

{ #category : #executing }
SindarinSaveScriptCommand >> execute [
	self context saveScript
]
