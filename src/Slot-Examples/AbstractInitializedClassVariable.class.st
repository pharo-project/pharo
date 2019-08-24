"
Intialized Class Variables define a default value as part of their definition.

The default parameter can be literal or a block.

There are two subclasses: setting the default value on class creation and lazy initialization on read.
"
Class {
	#name : #AbstractInitializedClassVariable,
	#superclass : #LiteralVariable,
	#instVars : [
		'default',
		'isBlock'
	],
	#category : #'Slot-Examples-ClassVariables'
}

{ #category : #accessing }
AbstractInitializedClassVariable >> default: anObject [
	default := anObject.
	isBlock := false
]

{ #category : #accessing }
AbstractInitializedClassVariable >> defaultBlock: aBlock [
	default := aBlock.
	isBlock := true
]

{ #category : #printing }
AbstractInitializedClassVariable >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name.
	isBlock 
		ifTrue: [aStream nextPutAll: ' defaultBlock: ']
		ifFalse: [aStream nextPutAll: ' default: '].
	default printOn: aStream.
]
