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

{ #category : 'stack' }
OCIRPushRemoteTemp >> deltaStack [

	^ 1
]

{ #category : 'testing' }
OCIRPushRemoteTemp >> isRemovableByPop [

	^ true
]
