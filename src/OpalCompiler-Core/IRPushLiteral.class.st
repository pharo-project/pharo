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
IRPushLiteral >> accept: aVisitor [
	^ aVisitor visitPushLiteral: self
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
IRPushLiteral >> isPushLiteral: valueTest [
	^ valueTest value: literal
]

{ #category : 'testing' }
IRPushLiteral >> isRemovableByPop [
	"Be conservative to keep symbols and other things"

	^ (#( nil true false ) includes: literal) or: [ literal isNumber ]
]

{ #category : 'accessing' }
IRPushLiteral >> literal [

	^ literal
]

{ #category : 'accessing' }
IRPushLiteral >> literal: object [

	literal := object
]

{ #category : 'accessing' }
IRPushLiteral >> quickRetNode [
	^ IRReturnLiteral new
		literal: literal;
		yourself
]
