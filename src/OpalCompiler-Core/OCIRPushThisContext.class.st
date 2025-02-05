"
I model push thisContext bytecode
"
Class {
	#name : 'OCIRPushThisContext',
	#superclass : 'OCIRAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushThisContext >> accept: aVisitor [
	^ aVisitor visitPushThisContext: self
]

{ #category : 'testing' }
OCIRPushThisContext >> isRemovableByPop [

	^ true
]
