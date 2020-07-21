"
I model thisContext variables
"
Class {
	#name : #OCThisContextVariable,
	#superclass : #OCSpecialVariable,
	#category : #'Deprecated90-OpalCompiler-Core'
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

{ #category : #debugging }
OCThisContextVariable >> readInContext: aContext [
	^aContext
]
