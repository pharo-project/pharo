"
I represent a shared pool of a class. Shared are objects shared between classes.
"
Class {
	#name : 'CDSharedPoolNode',
	#superclass : 'CDNode',
	#instVars : [
		'name',
		'start',
		'stop'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'instance creation' }
CDSharedPoolNode class >> node: aLiteralValueNode name: aString start: anInteger stop: anInteger2 [
	^ self new
			node: aLiteralValueNode;
			name: aString;
			 start: anInteger;
			 stop: anInteger2;
			yourself
]

{ #category : 'accessing' }
CDSharedPoolNode >> name [
	^name
]

{ #category : 'accessing' }
CDSharedPoolNode >> name: aString [
	name := aString
]

{ #category : 'accessing' }
CDSharedPoolNode >> node: aLiteralValueNode [
	originalNode := aLiteralValueNode
]

{ #category : 'accessing' }
CDSharedPoolNode >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
CDSharedPoolNode >> stop: anInteger [
	stop := anInteger
]
