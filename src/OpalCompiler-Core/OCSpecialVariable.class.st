"
I model self, thisContext and super
"
Class {
	#name : #OCSpecialVariable,
	#superclass : #OCAbstractLocalVariable,
	#category : #OpalCompiler-Core-Semantics
}

{ #category : #emitting }
OCSpecialVariable >> emitStore: methodBuilder [

	self shouldNotImplement
]

{ #category : #emitting }
OCSpecialVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
	
]

{ #category : #testing }
OCSpecialVariable >> isSpecialVariable [
	^true
]

{ #category : #testing }
OCSpecialVariable >> isThisContext [
	^false
]

{ #category : #testing }
OCSpecialVariable >> isUninitialized [

	^ false
]

{ #category : #testing }
OCSpecialVariable >> isWritable [
	^ false
]
