"
push remote temp
"
Class {
	#name : 'IRPushRemoteTemp',
	#superclass : 'IRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushRemoteTemp >> accept: aVisitor [
	^ aVisitor visitPushRemoteTemp: self
]

{ #category : 'testing' }
IRPushRemoteTemp >> isRemovableByPop [

	^ true
]
