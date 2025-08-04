"
specific node when returning self
"
Class {
	#name : 'OCIRReturnReceiver',
	#superclass : 'OCIRReturn',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRReturnReceiver >> accept: aVisitor [
	^ aVisitor visitReturnReceiver: self
]
