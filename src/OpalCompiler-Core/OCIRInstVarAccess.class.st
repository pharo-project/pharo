"
I am a bytecode accessing an instance variable. As such, I have an index.
"
Class {
	#name : 'OCIRInstVarAccess',
	#superclass : 'OCIRAccess',
	#instVars : [
		'index'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'accessing' }
OCIRInstVarAccess >> index [
	^ index
]

{ #category : 'accessing' }
OCIRInstVarAccess >> index: anInteger [
	index := anInteger
]

{ #category : 'testing' }
OCIRInstVarAccess >> isInstVarAccess [
	^ true
]
