"
An annotated paragraph is represented by: @@ in pillar syntax.

@@foo bar zork
"
Class {
	#name : #PRAnnotatedParagraph,
	#superclass : #PRParagraph,
	#instVars : [
		'annotation'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #'instance creation' }
PRAnnotatedParagraph class >> withAll: aCollection annotation: aString [
	^ (self withAll: aCollection)
		annotation: aString;
		yourself
]

{ #category : #comparing }
PRAnnotatedParagraph >> = anObject [
	^ super = anObject and: [ self annotation = anObject annotation ]
]

{ #category : #visiting }
PRAnnotatedParagraph >> accept: aVisitor [
	^ aVisitor visitAnnotatedParagraph: self
]

{ #category : #accessing }
PRAnnotatedParagraph >> annotation [
	^ annotation
]

{ #category : #accessing }
PRAnnotatedParagraph >> annotation: aString [
	annotation := aString
]

{ #category : #comparing }
PRAnnotatedParagraph >> hash [
	^ super hash bitXor: self annotation hash
]
