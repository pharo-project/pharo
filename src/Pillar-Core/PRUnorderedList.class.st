"
I am an unordered list. I am typically used for unnumbered lists
"
Class {
	#name : #PRUnorderedList,
	#superclass : #PRList,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRUnorderedList >> accept: aVisitor [
	^ aVisitor visitUnorderedList: self
]
