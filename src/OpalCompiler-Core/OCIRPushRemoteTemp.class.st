"
push remote temp
"
Class {
	#name : 'OCIRPushRemoteTemp',
	#superclass : 'OCIRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushRemoteTemp >> accept: aVisitor [
	^ aVisitor visitPushRemoteTemp: self
]

{ #category : 'testing' }
OCIRPushRemoteTemp >> isRemovableByPop [

	^ true
]
