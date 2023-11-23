"
push temp
"
Class {
	#name : 'IRPushTemp',
	#superclass : 'IRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushTemp >> accept: aVisitor [
	^ aVisitor visitPushTemp: self
]

{ #category : 'testing' }
IRPushTemp >> isRemovableByPop [

	^ true
]
