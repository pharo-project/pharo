"
STONReference holds a forward reference to another object during materialization.

"
Class {
	#name : #STONReference,
	#superclass : #Object,
	#instVars : [
		'index'
	],
	#category : #'STON-Core-Reader'
}

{ #category : #'instance creation' }
STONReference class >> index: integer [
	^ self new
		index: integer;
		yourself
]

{ #category : #comparing }
STONReference >> = anObject [
	^ self class == anObject class and: [ self index = anObject index ]
]

{ #category : #comparing }
STONReference >> hash [
	^ index hash
]

{ #category : #accessing }
STONReference >> index [
	^ index
]

{ #category : #accessing }
STONReference >> index: integer [
	index := integer
]

{ #category : #testing }
STONReference >> isStonReference [
	^ true
]

{ #category : #printing }
STONReference >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(; print: index; nextPut: $)
]
