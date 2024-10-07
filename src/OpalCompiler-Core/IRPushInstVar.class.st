"
push inst var
"
Class {
	#name : 'IRPushInstVar',
	#superclass : 'IRInstVarAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushInstVar >> accept: arg1 [

	^ arg1 visitPushInstVar: self
]

{ #category : 'testing' }
IRPushInstVar >> canBeQuickReturn [

	^ true
]

{ #category : 'testing' }
IRPushInstVar >> isRemovableByPop [

	^ true
]

{ #category : 'accessing' }
IRPushInstVar >> quickRetNode [

	| tmp1 |
	tmp1 := IRReturnInstVar new.
	tmp1 index: index.
	^ tmp1 yourself
]
