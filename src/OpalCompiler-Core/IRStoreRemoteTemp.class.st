"
store remote temp
"
Class {
	#name : #IRStoreRemoteTemp,
	#superclass : #IRRemoteTempAccess,
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #visiting }
IRStoreRemoteTemp >> accept: aVisitor [
	^ aVisitor visitStoreRemoteTemp: self
]

{ #category : #testing }
IRStoreRemoteTemp >> isStore [
	^ true
]

{ #category : #accessing }
IRStoreRemoteTemp >> popIntoNode [
	^ IRPopIntoRemoteTemp new
		name: name;
		tempVectorName: tempVectorName;
		yourself
]
