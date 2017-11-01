"
specific node when returning an inst var
"
Class {
	#name : #IRReturnInstVar,
	#superclass : #IRReturn,
	#instVars : [
		'index'
	],
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRReturnInstVar >> accept: aVisitor [
	^ aVisitor visitReturnInstVar: self
]

{ #category : #accessing }
IRReturnInstVar >> index [
	^ index
]

{ #category : #accessing }
IRReturnInstVar >> index: anObject [
	index := anObject
]
