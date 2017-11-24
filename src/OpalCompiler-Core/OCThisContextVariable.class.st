"
I model thisContext variables
"
Class {
	#name : #OCThisContextVariable,
	#superclass : #OCSpecialVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCThisContextVariable class >> semanticNodeClass [

	^RBThisContextNode 
]

{ #category : #emitting }
OCThisContextVariable >> emitValue: methodBuilder [

	methodBuilder pushThisContext
]

{ #category : #initialization }
OCThisContextVariable >> initialize [
	super initialize.
	
	name := 'thisContext'
]

{ #category : #testing }
OCThisContextVariable >> isThisContext [
	^true
]
