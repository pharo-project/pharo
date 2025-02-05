"
store remote temp
"
Class {
	#name : 'OCIRStoreRemoteTemp',
	#superclass : 'OCIRRemoteTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRStoreRemoteTemp >> accept: aVisitor [
	^ aVisitor visitStoreRemoteTemp: self
]

{ #category : 'testing' }
OCIRStoreRemoteTemp >> isStore [
	^ true
]

{ #category : 'accessing' }
OCIRStoreRemoteTemp >> popIntoNode [
	^ OCIRPopIntoRemoteTemp new
		name: name;
		tempVectorName: tempVectorName;
		yourself
]
