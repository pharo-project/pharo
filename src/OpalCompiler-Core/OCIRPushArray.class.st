"
I model the pushArray bytecode.

Used for setting up the temp vectors and for the brace array construct: { }.
"
Class {
	#name : 'OCIRPushArray',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'size',
		'cons'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushArray >> accept: aVisitor [
	^ aVisitor visitPushArray: self
]

{ #category : 'accessing' }
OCIRPushArray >> cons [
	^ cons
]

{ #category : 'accessing' }
OCIRPushArray >> cons: aBool [
	cons := aBool
]

{ #category : 'initialization' }
OCIRPushArray >> initialize [
	size := 0.
	cons := false
]

{ #category : 'accessing' }
OCIRPushArray >> size [
	^ size
]

{ #category : 'accessing' }
OCIRPushArray >> size: aSmallInteger [
	size := aSmallInteger
]
