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

{ #category : #accessing }
IRJumpIf >> boolean [

	^ boolean
]

{ #category : #accessing }
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

{ #category : #accessing }
IRJumpIf >> nextBytecodeOffsetAfterJump [
	"check if we are in ifTrue:ifFalse: / ifFalse:ifTrue: or in ifTrue: / ifFalse: and answers the next byte code offset"
	^destination last isJump 
		ifTrue: [ destination last destination first bytecodeOffset ]
		ifFalse: [ 
			(destination sequence at: ((destination size - 1) max: 1)) bytecodeOffset ]
]

{ #category : #accessing }
IRJumpIf >> nonBodySuccessorSequences [

	^ {destination}
]

{ #category : #accessing }
IRJumpIf >> otherwise [

	^ otherwise
]

{ #category : #accessing }
IRJumpIf >> otherwise: aSequence [

	otherwise := aSequence
]

{ #category : #accessing }
IRJumpIf >> successorSequences [

	^ {destination. otherwise}
]
