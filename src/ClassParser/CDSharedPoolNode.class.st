"
I represent a shared pool of a class. Shared are objects shared between classes.
"
Class {
	#name : #CDSharedPoolNode,
	#superclass : #CDNode,
	#instVars : [
		'name',
		'start',
		'stop'
	],
	#category : #'ClassParser-Model'
}

{ #category : #'instance creation' }
CDSharedPoolNode class >> node: aRBLiteralValueNode name: aString start: anInteger stop: anInteger2 [
	^ self new
			node: aRBLiteralValueNode;
			name: aString;
			 start: anInteger;
			 stop: anInteger2;
			yourself
]

{ #category : #accessing }
CDSharedPoolNode >> name [
	^name 
]

{ #category : #accessing }
CDSharedPoolNode >> name: aString [ 
	name := aString
]

{ #category : #accessing }
CDSharedPoolNode >> node: aRBLiteralValueNode [ 
	originalNode := aRBLiteralValueNode
]

{ #category : #accessing }
CDSharedPoolNode >> start: anInteger [ 
	start := anInteger
]

{ #category : #accessing }
CDSharedPoolNode >> stop: anInteger [ 
	stop := anInteger 
]
