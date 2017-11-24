"
I am a bytecode accessing an instance variable. As such, I have an index.
"
Class {
	#name : #IRInstVarAccess,
	#superclass : #IRAccess,
	#instVars : [
		'index'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #accessing }
IRInstVarAccess >> index [
	^ index
]

{ #category : #accessing }
IRInstVarAccess >> index: anInteger [
	index := anInteger
]

{ #category : #testing }
IRInstVarAccess >> isInstVarAccess [
	^ true
]
