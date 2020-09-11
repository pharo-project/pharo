"
I model self variables 
"
Class {
	#name : #OCSelfVariable,
	#superclass : #OCSpecialVariable,
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #emitting }
OCSelfVariable >> emitValue: methodBuilder [

	methodBuilder pushReceiver
]

{ #category : #initialization }
OCSelfVariable >> initialize [
	super initialize.
	
	name := 'self'
]

{ #category : #debugging }
OCSelfVariable >> readInContext: aContext [
	^aContext receiver
]
