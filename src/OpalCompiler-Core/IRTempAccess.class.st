"
I model the pushTemporary Bytecode
"
Class {
	#name : #IRTempAccess,
	#superclass : #IRAccess,
	#instVars : [
		'name'
	],
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #testing }
IRTempAccess >> isRemoteTemp [
	^ false
]

{ #category : #testing }
IRTempAccess >> isTemp [
	^true.
]

{ #category : #accessing }
IRTempAccess >> name [

	^name
]

{ #category : #accessing }
IRTempAccess >> name: aString [
	name := aString
]
