"
I model the bytecode for block returns.
"
Class {
	#name : #IRBlockReturnTop,
	#superclass : #IRReturn,
	#instVars : [
		'successor'
	],
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #visiting }
IRBlockReturnTop >> accept: aVisitor [
	^ aVisitor visitBlockReturnTop: self
]

{ #category : #testing }
IRBlockReturnTop >> isBlockReturnTop [

	^true.
]

{ #category : #accessing }
IRBlockReturnTop >> successor [
	^ successor
]

{ #category : #accessing }
IRBlockReturnTop >> successor: anObject [

	successor := anObject. 
]

{ #category : #accessing }
IRBlockReturnTop >> successorSequences [
	"sent to last instruction in sequence which is expected to be a jump and return instruction"

	^  { successor }
]
