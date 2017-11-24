"
I am a filter for a token.

If a node item contains my token I accept it
"
Class {
	#name : #TokenTreeFilter,
	#superclass : #AbstractTreeFilter,
	#instVars : [
		'token'
	],
	#category : #'Spec-Core-Widgets-Support'
}

{ #category : #'instance creation' }
TokenTreeFilter class >> token: token [

	^ self new
		token: token;
		yourself
]

{ #category : #initialization }
TokenTreeFilter >> initialize [

	super initialize.

	token := ''.
]

{ #category : #filtering }
TokenTreeFilter >> keepTreeNode: aNode [

	^ aNode matchTokenFilter: self
]

{ #category : #accessing }
TokenTreeFilter >> token [
	^ token
]

{ #category : #accessing }
TokenTreeFilter >> token: anObject [
	
	token := anObject
]
