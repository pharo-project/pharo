"
pop into remote temp
"
Class {
	#name : 'IRPopIntoRemoteTemp',
	#superclass : 'IRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPopIntoRemoteTemp >> accept: aVisitor [
	^ aVisitor visitPopIntoRemoteTemp: self
]
