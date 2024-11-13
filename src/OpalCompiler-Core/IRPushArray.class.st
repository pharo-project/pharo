"
I model the pushArray bytecode.

Used for setting up the temp vectors and for the brace array construct: { }.
"
Class {
	#name : 'IRPushArray',
	#superclass : 'IRInstruction',
	#instVars : [
		'size',
		'initsFromStack'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushArray >> accept: aVisitor [
	^ aVisitor visitPushArray: self
]

{ #category : 'initialization' }
IRPushArray >> initialize [
	size := 0.
	initsFromStack := false
]

{ #category : 'accessing' }
IRPushArray >> initsFromStack [
	^ initsFromStack
]

{ #category : 'accessing' }
IRPushArray >> initsFromStack: aBool [
	initsFromStack := aBool
]

{ #category : 'accessing' }
IRPushArray >> size [
	^ size
]

{ #category : 'accessing' }
IRPushArray >> size: aSmallInteger [
	size := aSmallInteger
]
