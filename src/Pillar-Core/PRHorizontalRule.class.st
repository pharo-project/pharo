"
I am a horizontal rule.
"
Class {
	#name : #PRHorizontalRule,
	#superclass : #PRDocumentItem,
	#category : #'Pillar-Core'
}

{ #category : #testing }
PRHorizontalRule class >> isAbstract [
	^ false
]

{ #category : #visiting }
PRHorizontalRule >> accept: aVisitor [
	^ aVisitor visitHorizontalRule: self
]
