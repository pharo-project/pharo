"
Instruction ""if: boolean goto: labelNum1 otherwise: labelNum2""
"
Class {
	#name : #IRJumpIf,
	#superclass : #IRJump,
	#instVars : [
		'boolean',
		'otherwise'
	],
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRJumpIf >> accept: aVisitor [
	^ aVisitor visitJumpIf: self
]

{ #category : #acessing }
IRJumpIf >> boolean [

	^ boolean
]

{ #category : #acessing }
IRJumpIf >> boolean: bool [

	boolean := bool
]

{ #category : #testing }
IRJumpIf >> isGoto [
	"is unconditional jump"

	^ false
]

{ #category : #testing }
IRJumpIf >> isIf [

	^ true
]

{ #category : #acessing }
IRJumpIf >> nextBytecodeOffsetAfterJump [
	"check if we are in ifTrue:ifFalse: / ifFalse:ifTrue: or in ifTrue: / ifFalse: and answers the next byte code offset"
	^destination last isJump 
		ifTrue: [ destination last destination first bytecodeOffset ]
		ifFalse: [ 
			(destination sequence at: ((destination size - 1) max: 1)) bytecodeOffset ]
]

{ #category : #acessing }
IRJumpIf >> nonBodySuccessorSequences [

	^ {destination}
]

{ #category : #acessing }
IRJumpIf >> otherwise [

	^ otherwise
]

{ #category : #acessing }
IRJumpIf >> otherwise: aSequence [

	otherwise := aSequence
]

{ #category : #acessing }
IRJumpIf >> successorSequences [

	^ {destination. otherwise}
]
