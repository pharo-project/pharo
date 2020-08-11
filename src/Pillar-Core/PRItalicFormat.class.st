"
Format can be nested.

`""""bold ''italic''bold""""` shows **bold_italic_bold**
"
Class {
	#name : #PRItalicFormat,
	#superclass : #PRFormat,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRItalicFormat >> accept: aVisitor [
	^ aVisitor visitItalicFormat: self
]
