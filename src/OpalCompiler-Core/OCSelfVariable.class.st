"
I model self variables 
"
Class {
	#name : #OCSelfVariable,
	#superclass : #OCSpecialVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCSelfVariable class >> semanticNodeKind [

	^RBSelfKind
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
