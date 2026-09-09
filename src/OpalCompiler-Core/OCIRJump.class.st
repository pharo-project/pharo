"
Instruction ""goto: labelNum""
"
Class {
	#name : 'OCIRJump',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'destination',
		'successor'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRJump >> accept: aVisitor [
	^ aVisitor visitJump: self
]

{ #category : 'accessing' }
OCIRJump >> destination [

	^ destination
]

{ #category : 'accessing' }
OCIRJump >> destination: aSequence [
	destination := aSequence
]

{ #category : 'testing' }
OCIRJump >> isGoto [
	"is unconditional jump"

	^ true
]

{ #category : 'testing' }
OCIRJump >> isJump [
	"goto or if"

	^ true
]

{ #category : 'accessing' }
OCIRJump >> nextBytecodeOffsetAfterJump [
	^ destination last destination first bytecodeOffset
]

{ #category : 'accessing' }
OCIRJump >> successor [
	^successor
]

{ #category : 'accessing' }
OCIRJump >> successor: succ [
	successor := succ
]

{ #category : 'accessing' }
OCIRJump >> successorSequences [

	^ {destination. successor}
]
