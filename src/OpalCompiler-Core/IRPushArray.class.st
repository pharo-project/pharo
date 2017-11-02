"
I model the pushArray bytecode.

Used for setting up the temp vectors and for the brace array construct: { }.
"
Class {
	#name : #IRPushArray,
	#superclass : #IRInstruction,
	#instVars : [
		'size',
		'cons'
	],
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushArray >> accept: aVisitor [
	^ aVisitor visitPushArray: self
]

{ #category : #accessing }
IRPushArray >> cons [
	^ cons
]

{ #category : #accessing }
IRPushArray >> cons: aBool [
	cons := aBool.
]

{ #category : #initialization }
IRPushArray >> initialize [
	size := 0.
	cons := false.
	
]

{ #category : #accessing }
IRPushArray >> size [ 
	^ size
]

{ #category : #accessing }
IRPushArray >> size: aSmallInteger [ 
	size := aSmallInteger
]
