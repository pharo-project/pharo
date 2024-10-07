"
Fix the IR nodes.

Transforms:

store, pop => popInto
some returns => quick returns
"
Class {
	#name : 'IRFix',
	#superclass : 'IRVisitor',
	#instVars : [
		'prevInstr',
		'storePopToFix',
		'retToFix'
	],
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'private' }
IRFix >> convertRet: arg1 forSeq: arg2 [

	| tmp1 tmp2 |
	tmp1 := arg1 key.
	tmp2 := arg1 value.
	arg2
		replaceNode: tmp1
		withNode: (tmp1 quickRetNode sourceNode: tmp2 sourceNode);
		remove: tmp2
]

{ #category : 'private' }
IRFix >> convertStorePop: arg1 forSeq: arg2 [

	| tmp1 tmp2 |
	tmp1 := arg1 key.
	tmp2 := arg1 value.
	arg2
		replaceNode: tmp1
		withNode: (tmp1 popIntoNode sourceNode: tmp1 sourceNode);
		remove: tmp2
]

{ #category : 'initialization' }
IRFix >> initialize [

	super initialize.
	storePopToFix := OrderedCollection new.
	retToFix := OrderedCollection new
]

{ #category : 'visiting' }
IRFix >> visitInstruction: arg1 [

	self visitNode: arg1.
	prevInstr := arg1
]

{ #category : 'visiting' }
IRFix >> visitPop: arg1 [

	prevInstr ifNil: [ ^ self ] ifNotNil: [ prevInstr ].
	prevInstr isStore ifFalse: [ ^ self ].
	storePopToFix add: prevInstr -> arg1
]

{ #category : 'visiting' }
IRFix >> visitReturn: arg1 [

	prevInstr ifNil: [ ^ self ] ifNotNil: [ prevInstr ].
	prevInstr canBeQuickReturn ifFalse: [ ^ self ].
	retToFix add: prevInstr -> arg1
]

{ #category : 'visiting' }
IRFix >> visitSequence: arg1 [

	prevInstr := nil.
	storePopToFix reset.
	retToFix reset.
	super visitSequence: arg1.
	retToFix do: [ :arg2 | self convertRet: arg2 forSeq: arg1 ].
	storePopToFix do: [ :arg3 | self convertStorePop: arg3 forSeq: arg1 ]
]
