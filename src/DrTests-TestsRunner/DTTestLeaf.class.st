"
I am a leaf of the results tree for the test-runner plugin.

Additionally to my superclass, I store the type of the result.
"
Class {
	#name : #DTTestLeaf,
	#superclass : #DTTreeLeaf,
	#instVars : [
		'type'
	],
	#category : #'DrTests-TestsRunner'
}

{ #category : #'instance creation' }
DTTestLeaf class >> content: anObject type: aTestResultType [
	^ (self content: anObject)
		type: aTestResultType;
		yourself
]

{ #category : #accessing }
DTTestLeaf >> type [
	^ type
]

{ #category : #accessing }
DTTestLeaf >> type: anObject [
	type := anObject
]
