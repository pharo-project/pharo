"
I model the pushRemoteTemporary Bytecode
"
Class {
	#name : #IRRemoteTempAccess,
	#superclass : #IRTempAccess,
	#instVars : [
		'tempVectorName'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #testing }
IRRemoteTempAccess >> isRemoteTemp [
	^ true
]

{ #category : #accessing }
IRRemoteTempAccess >> tempVectorName [
	^ tempVectorName
]

{ #category : #accessing }
IRRemoteTempAccess >> tempVectorName: anObject [
	tempVectorName := anObject
]
