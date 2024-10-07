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
IRStoreInstVar >> accept: arg1 [

	^ arg1 visitStoreInstVar: self
]

{ #category : 'testing' }
IRStoreInstVar >> isStore [

	^ true
]

{ #category : 'accessing' }
IRStoreInstVar >> popIntoNode [

	| tmp2 |
	tmp2 := IRPopIntoInstVar new.
	tmp2 index: index.
	^ tmp2 yourself
]
