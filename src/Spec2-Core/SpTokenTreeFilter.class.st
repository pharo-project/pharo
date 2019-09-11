"
I am a filter for a token.

If a node item contains my token I accept it
"
Class {
	#name : #SpTokenTreeFilter,
	#superclass : #SpAbstractTreeFilter,
	#instVars : [
		'token'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #'instance creation' }
SpTokenTreeFilter class >> token: token [

	^ self new
		token: token;
		yourself
]

{ #category : #initialization }
SpTokenTreeFilter >> initialize [

	super initialize.

	token := ''.
]

{ #category : #filtering }
SpTokenTreeFilter >> keepTreeNode: aNode [

	^ aNode matchTokenFilter: self
]

{ #category : #accessing }
SpTokenTreeFilter >> token [
	^ token
]

{ #category : #accessing }
SpTokenTreeFilter >> token: anObject [
	
	token := anObject
]
