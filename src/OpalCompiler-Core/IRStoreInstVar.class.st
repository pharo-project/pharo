"
store inst var
"
Class {
	#name : 'IRStoreInstVar',
	#superclass : 'IRInstVarAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRStoreInstVar >> accept: aVisitor [
	^ aVisitor visitStoreInstVar: self
]

{ #category : 'testing' }
IRStoreInstVar >> isStore [
	^ true
]

{ #category : 'accessing' }
IRStoreInstVar >> popIntoNode [
	^ IRPopIntoInstVar new
		index: index;
		yourself
]
