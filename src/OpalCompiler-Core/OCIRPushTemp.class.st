"
push temp
"
Class {
	#name : 'OCIRPushTemp',
	#superclass : 'OCIRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushTemp >> accept: aVisitor [
	^ aVisitor visitPushTemp: self
]

{ #category : 'testing' }
OCIRPushTemp >> isRemovableByPop [

	^ true
]
