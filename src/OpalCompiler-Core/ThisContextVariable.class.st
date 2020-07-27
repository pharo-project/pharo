"
I model thisContext keyword
"
Class {
	#name : #ThisContextVariable,
	#superclass : #ReservedVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
ThisContextVariable class >> semanticNodeClass [

	^RBThisContextNode 
]

{ #category : #emitting }
ThisContextVariable >> emitValue: methodBuilder [

	methodBuilder pushThisContext
]

{ #category : #initialization }
ThisContextVariable >> initialize [
	super initialize.
	
	name := 'thisContext'
]

{ #category : #testing }
ThisContextVariable >> isThisContextVariable [
	^true
]

{ #category : #debugging }
ThisContextVariable >> readInContext: aContext [
	^aContext
]

{ #category : #queries }
ThisContextVariable >> usingMethods [
	^ SystemNavigation new allMethods select: [ :method | 
		  method readsThisContext ]
]
