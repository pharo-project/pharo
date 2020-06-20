"
I model self variables 
"
Class {
	#name : #OCSelfVariable,
	#superclass : #OCSpecialVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCSelfVariable class >> semanticNodeClass [

	^RBSelfNode 
]

{ #category : #emitting }
OCSelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : #initialization }
OCSelfVariable >> initialize [
	super initialize.
	
	name := 'self'
]

{ #category : #testing }
OCSelfVariable >> isSelf [
	^true
]

{ #category : #testing }
OCSelfVariable >> isSelfOrSuper [
	^true
]

{ #category : #debugging }
OCSelfVariable >> readInContext: aContext [
	^aContext receiver
]
