"
store inst var
"
Class {
	#name : 'OCIRStoreInstVar',
	#superclass : 'OCIRInstVarAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRStoreInstVar >> accept: aVisitor [
	^ aVisitor visitStoreInstVar: self
]

{ #category : 'testing' }
OCIRStoreInstVar >> isStore [
	^ true
]

{ #category : 'accessing' }
OCIRStoreInstVar >> popIntoNode [
	^ OCIRPopIntoInstVar new
		index: index;
		yourself
]
