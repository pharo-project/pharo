"
I am a simple wrapper for patches
"
Class {
	#name : #KomitPatch,
	#superclass : #Object,
	#instVars : [
		'base',
		'patch'
	],
	#category : #Komitter-Models
}

{ #category : #'instance creation' }
KomitPatch class >> base: base patch: patch [

	^ self new
		base: base;
		patch: patch;
		yourself
]

{ #category : #accessing }
KomitPatch >> base [
	^ base ifNil: [ MCSnapshot empty ]
]

{ #category : #accessing }
KomitPatch >> base: anObject [
	base := anObject
]

{ #category : #protocol }
KomitPatch >> operations [

	^ patch operations
]

{ #category : #accessing }
KomitPatch >> patch [
	^ patch
]

{ #category : #accessing }
KomitPatch >> patch: anObject [
	patch := anObject
]
