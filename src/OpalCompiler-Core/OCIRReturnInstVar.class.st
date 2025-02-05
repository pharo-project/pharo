"
specific node when returning an inst var
"
Class {
	#name : 'OCIRReturnInstVar',
	#superclass : 'OCIRReturn',
	#instVars : [
		'index'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRReturnInstVar >> accept: aVisitor [
	^ aVisitor visitReturnInstVar: self
]

{ #category : 'accessing' }
OCIRReturnInstVar >> index [
	^ index
]

{ #category : 'accessing' }
OCIRReturnInstVar >> index: anObject [
	index := anObject
]
