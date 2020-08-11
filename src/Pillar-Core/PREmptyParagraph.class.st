"
An empty paragraph
"
Class {
	#name : #PREmptyParagraph,
	#superclass : #PRParagraph,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PREmptyParagraph >> accept: aVisitor [
	^ aVisitor visitEmptyParagraph: self
]
