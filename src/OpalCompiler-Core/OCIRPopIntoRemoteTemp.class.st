"
pop into remote temp
"
Class {
	#name : 'OCIRPopIntoRemoteTemp',
	#superclass : 'OCIRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPopIntoRemoteTemp >> accept: aVisitor [
	^ aVisitor visitPopIntoRemoteTemp: self
]
