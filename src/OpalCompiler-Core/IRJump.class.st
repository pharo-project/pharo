"
Instruction ""goto: labelNum""
"
Class {
	#name : #IRJump,
	#superclass : #IRInstruction,
	#instVars : [
		'destination',
		'successor'
	],
	#category : #OpalCompiler-Core-IR-Nodes
}

{ #category : #visiting }
IRJump >> accept: aVisitor [
	^ aVisitor visitJump: self
]

{ #category : #accessing }
IRJump >> destination [

	^ destination
]

{ #category : #accessing }
IRJump >> destination: aSequence [
	destination := aSequence
]

{ #category : #testing }
IRJump >> isGoto [
	"is unconditional jump"

	^ true
]

{ #category : #testing }
IRJump >> isJump [
	"goto or if"

	^ true
]

{ #category : #accessing }
IRJump >> successor [
	^successor
]

{ #category : #accessing }
IRJump >> successor: succ [
	successor := succ
]

{ #category : #accessing }
IRJump >> successorSequences [

	^ {destination. successor} 
]
