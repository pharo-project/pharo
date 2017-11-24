"
push remote temp
"
Class {
	#name : #IRPushRemoteTemp,
	#superclass : #IRRemoteTempAccess,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushRemoteTemp >> accept: aVisitor [
	^ aVisitor visitPushRemoteTemp: self
]
