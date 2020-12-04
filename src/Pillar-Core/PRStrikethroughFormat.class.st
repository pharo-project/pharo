"
Strikethrough format
"
Class {
	#name : #PRStrikethroughFormat,
	#superclass : #PRFormat,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRStrikethroughFormat >> accept: aVisitor [
	^ aVisitor visitStrikethroughFormat: self
]
