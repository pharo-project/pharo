Class {
	#name : 'OCIRPushLiteralAt',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'index'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushLiteralAt >> accept: aVisitor [ 

	^ aVisitor visitPushLiteralAt: self
]

{ #category : 'accessing' }
OCIRPushLiteralAt >> index [
	^ index
]

{ #category : 'accessing' }
OCIRPushLiteralAt >> index: anInteger [ 
	index := anInteger
]
