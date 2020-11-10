"
I model thisContext keyword
"
Class {
	#name : #ThisContextVariable,
	#superclass : #ReservedVariable,
	#category : #'Kernel-Variables'
}

{ #category : #visiting }
ThisContextVariable >> acceptVisitor: aProgramNodeVisitor node: aNode [
	^ aProgramNodeVisitor visitThisContextNode: aNode
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
	^ environment allMethods select: [ :method | 
		  method readsThisContext ]
]
