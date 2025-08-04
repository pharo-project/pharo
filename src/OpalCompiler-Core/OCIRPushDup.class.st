"
I'm quite badly named. 

Instruction ""pushDup"" I'm basically emitting a bytecode that duplicate the top stack.
"
Class {
	#name : 'OCIRPushDup',
	#superclass : 'OCIRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushDup >> accept: aVisitor [
	^ aVisitor visitPushDup: self
]

{ #category : 'testing' }
OCIRPushDup >> isRemovableByPop [

	^ true
]
