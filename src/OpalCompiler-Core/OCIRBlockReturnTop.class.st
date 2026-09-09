"
I model the bytecode for block returns.
"
Class {
	#name : 'OCIRBlockReturnTop',
	#superclass : 'OCIRReturn',
	#instVars : [
		'successor'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRBlockReturnTop >> accept: aVisitor [
	^ aVisitor visitBlockReturnTop: self
]

{ #category : 'testing' }
OCIRBlockReturnTop >> isBlockReturnTop [

	^true
]

{ #category : 'accessing' }
OCIRBlockReturnTop >> successor [
	^ successor
]

{ #category : 'accessing' }
OCIRBlockReturnTop >> successor: anObject [

	successor := anObject
]

{ #category : 'accessing' }
OCIRBlockReturnTop >> successorSequences [
	"sent to last instruction in sequence which is expected to be a jump and return instruction"

	^  { successor }
]
