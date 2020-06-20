"
I model self, thisContext and super
"
Class {
	#name : #OCSpecialVariable,
	#superclass : #OCAbstractLocalVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #emitting }
OCSpecialVariable >> emitStore: methodBuilder [

	self shouldNotImplement
]

{ #category : #testing }
OCSpecialVariable >> isSpecialVariable [
	^true
]

{ #category : #testing }
OCSpecialVariable >> isUninitialized [

	^ false
]

{ #category : #testing }
OCSpecialVariable >> isWritable [
	^ false
]

{ #category : #debugging }
OCSpecialVariable >> write: aValue toContext: aContext [
	
	self error: name, ' is reserved word and cant be modified'
]
