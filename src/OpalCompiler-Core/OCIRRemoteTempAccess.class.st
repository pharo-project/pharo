"
I model the pushRemoteTemporary bytecode. In particular for temporary that outlives their existing context
and are stored into so called tempVector.
"
Class {
	#name : 'OCIRRemoteTempAccess',
	#superclass : 'OCIRTempAccess',
	#instVars : [
		'tempVectorName'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'testing' }
OCIRRemoteTempAccess >> isRemoteTemp [
	^ true
]

{ #category : 'accessing' }
OCIRRemoteTempAccess >> tempVectorName [
	^ tempVectorName
]

{ #category : 'accessing' }
OCIRRemoteTempAccess >> tempVectorName: anObject [
	tempVectorName := anObject
]
