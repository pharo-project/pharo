"
I model ""self"" keyword 
"
Class {
	#name : #SelfVariable,
	#superclass : #ReservedVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
SelfVariable class >> semanticNodeClass [

	^RBSelfNode 
]

{ #category : #emitting }
SelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : #initialization }
SelfVariable >> initialize [
	super initialize.
	
	name := 'self'
]

{ #category : #testing }
SelfVariable >> isSelfVariable [
	^true
]

{ #category : #debugging }
SelfVariable >> readInContext: aContext [
	^aContext receiver
]
