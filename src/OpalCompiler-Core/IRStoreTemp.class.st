"
store temp
"
Class {
	#name : #IRStoreTemp,
	#superclass : #IRTempAccess,
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #visiting }
IRStoreTemp >> accept: aVisitor [
	^ aVisitor visitStoreTemp: self
]

{ #category : #testing }
IRStoreTemp >> isStore [
	^ true
]

{ #category : #accessing }
IRStoreTemp >> popIntoNode [
	^ IRPopIntoTemp new
		name: name;
		yourself
]
