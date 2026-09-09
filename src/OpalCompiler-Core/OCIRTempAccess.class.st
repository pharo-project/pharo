"
I model the pushTemporary Bytecode
"
Class {
	#name : 'OCIRTempAccess',
	#superclass : 'OCIRAccess',
	#instVars : [
		'name'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'testing' }
OCIRTempAccess >> isRemoteTemp [
	^ false
]

{ #category : 'testing' }
OCIRTempAccess >> isTemp [
	^true
]

{ #category : 'accessing' }
OCIRTempAccess >> name [

	^name
]

{ #category : 'accessing' }
OCIRTempAccess >> name: aString [
	name := aString
]
