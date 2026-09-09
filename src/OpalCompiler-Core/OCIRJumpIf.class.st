"
Instruction ""if: boolean goto: labelNum1 otherwise: labelNum2""
"
Class {
	#name : 'OCIRJumpIf',
	#superclass : 'OCIRJump',
	#instVars : [
		'boolean',
		'otherwise'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRJumpIf >> accept: aVisitor [
	^ aVisitor visitJumpIf: self
]

{ #category : 'accessing' }
OCIRJumpIf >> boolean [

	^ boolean
]

{ #category : 'accessing' }
OCIRJumpIf >> boolean: bool [

	boolean := bool
]

{ #category : 'testing' }
OCIRJumpIf >> isGoto [
	"is unconditional jump"

	^ false
]

{ #category : 'testing' }
OCIRJumpIf >> isIf [

	^ true
]

{ #category : 'accessing' }
OCIRJumpIf >> nextBytecodeOffsetAfterJump [
	"check if we are in ifTrue:ifFalse: / ifFalse:ifTrue: or in ifTrue: / ifFalse: and answers the next byte code offset"
	^destination last isJump
		ifTrue: [ destination last destination first bytecodeOffset ]
		ifFalse: [
			(destination sequence at: ((destination size - 1) max: 1)) bytecodeOffset ]
]

{ #category : 'accessing' }
OCIRJumpIf >> nonBodySuccessorSequences [

	^ {destination}
]

{ #category : 'accessing' }
OCIRJumpIf >> otherwise [

	^ otherwise
]

{ #category : 'accessing' }
OCIRJumpIf >> otherwise: aSequence [

	otherwise := aSequence
]

{ #category : 'accessing' }
OCIRJumpIf >> successorSequences [

	^ {destination. otherwise}
]
