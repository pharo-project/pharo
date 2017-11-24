"
I model super variables
"
Class {
	#name : #OCSuperVariable,
	#superclass : #OCSpecialVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCSuperVariable class >> semanticNodeClass [

	^RBSuperNode 
]

{ #category : #initialization }
OCSuperVariable >> initialize [
	super initialize.
	
	name := 'super'
]

{ #category : #testing }
OCSuperVariable >> isSuper [
	^true
]
