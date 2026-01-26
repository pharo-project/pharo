"
Instruction ""popTop""
"
Class {
	#name : 'OCIRPop',
	#superclass : 'OCIRInstruction',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPop >> accept: aVisitor [
	^ aVisitor visitPop: self
]

{ #category : 'stack' }
OCIRPop >> deltaStack [

	^ -1
]

{ #category : 'testing' }
OCIRPop >> isPop [
	^true
]
