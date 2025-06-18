"
I represent the instruction ""pushLiteral: object"".

From the bytecode perspective: The bytecode will be either specialized for literals that are not placed in the method literal frame or the bytecode will refer to the literal in the method literal frame using an offset.
"
Class {
	#name : 'OCIRPushLiteral',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'literal'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushLiteral >> accept: aVisitor [
	^ aVisitor visitPushLiteral: self
]

{ #category : 'testing' }
OCIRPushLiteral >> canBeQuickReturn [

	"Put here whatever can be optimized as a ret literal or as a quick method.
	Same contents as quickPrimSpecialConstants, but this should require passing the encoder as argument all the way down"
	^ #( nil true false -1 0 1 2 ) includes: literal
]

{ #category : 'testing' }
OCIRPushLiteral >> isPushLiteral [

	^ true
]

{ #category : 'testing' }
OCIRPushLiteral >> isPushLiteral: valueTest [
	^ valueTest value: literal
]

{ #category : 'testing' }
OCIRPushLiteral >> isRemovableByPop [
	"Be conservative to keep symbols and other things"

	^ (#( nil true false ) includes: literal) or: [ literal isNumber ]
]

{ #category : 'accessing' }
OCIRPushLiteral >> literal [

	^ literal
]

{ #category : 'accessing' }
OCIRPushLiteral >> literal: object [

	literal := object
]

{ #category : 'accessing' }
OCIRPushLiteral >> quickRetNode [
	^ OCIRReturnLiteral new
		literal: literal;
		yourself
]
