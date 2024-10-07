"
Instruction ""pushLiteral: object""
"
Class {
	#name : 'IRPushLiteral',
	#superclass : 'IRInstruction',
	#instVars : [
		'literal'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushLiteral >> accept: arg1 [

	^ arg1 visitPushLiteral: self
]

{ #category : 'testing' }
IRPushLiteral >> canBeQuickReturn [

	^ true
]

{ #category : 'testing' }
IRPushLiteral >> isPushLiteral [

	^ true
]

{ #category : 'testing' }
IRPushLiteral >> isPushLiteral: arg1 [

	^ arg1 value: literal
]

{ #category : 'testing' }
IRPushLiteral >> isRemovableByPop [

	^ (#( nil true false ) includes: literal) or: [ literal isNumber ]
]

{ #category : 'accessing' }
IRPushLiteral >> literal [

	^ literal
]

{ #category : 'accessing' }
IRPushLiteral >> literal: arg1 [

	literal := arg1
]

{ #category : 'accessing' }
IRPushLiteral >> quickRetNode [

	| tmp2 |
	tmp2 := IRReturnLiteral new.
	tmp2 literal: literal.
	^ tmp2 yourself
]
