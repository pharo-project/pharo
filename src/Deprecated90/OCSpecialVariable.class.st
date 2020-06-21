"
I model self, thisContext and super
"
Class {
	#name : #OCSpecialVariable,
	#superclass : #OCAbstractLocalVariable,
	#category : #'Deprecated90-OpalCompiler-Core'
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
OCSpecialVariable >> write: aValue inContext: aContext [
	
	self error: name, ' is reserved word and cant be modified'
]
