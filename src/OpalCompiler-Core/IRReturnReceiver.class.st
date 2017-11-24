"
specific node when returning self
"
Class {
	#name : #IRReturnReceiver,
	#superclass : #IRReturn,
	#category : #'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRReturnReceiver >> accept: aVisitor [
	^ aVisitor visitReturnReceiver: self
]
