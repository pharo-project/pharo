"
I represent a header within a document. My level is a natural number.
"
Class {
	#name : #PRHeader,
	#superclass : #PRDocumentGroup,
	#instVars : [
		'level'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRHeader class >> withAll: aCollection level: anInteger [
	^ (self withAll: aCollection)
		level: anInteger;
		yourself
]

{ #category : #comparing }
PRHeader >> = anObject [
	^ super = anObject
		and: [ self level = anObject level ]
]

{ #category : #visiting }
PRHeader >> accept: aVisitor [
	^ aVisitor visitHeader: self
]

{ #category : #comparing }
PRHeader >> hash [
	^ super hash bitXor: self level hash
]

{ #category : #accessing }
PRHeader >> headerText [
	^ self children first text
]

{ #category : #testing }
PRHeader >> isHeader [
	^ true
]

{ #category : #accessing }
PRHeader >> level [
	^ level
]

{ #category : #accessing }
PRHeader >> level: anInteger [
	level := anInteger
]
