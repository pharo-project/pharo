"
store temp
"
Class {
	#name : 'OCIRStoreTemp',
	#superclass : 'OCIRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRStoreTemp >> accept: aVisitor [
	^ aVisitor visitStoreTemp: self
]

{ #category : 'testing' }
OCIRStoreTemp >> isStore [
	^ true
]

{ #category : 'accessing' }
OCIRStoreTemp >> popIntoNode [
	^ OCIRPopIntoTemp new
		name: name;
		yourself
]
