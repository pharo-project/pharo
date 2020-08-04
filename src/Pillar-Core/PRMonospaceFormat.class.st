"
In Pillar, text can be shown in bold by wrapping the text around `==`.

`==text in bold==` renders as `text in bold`.

"
Class {
	#name : #PRMonospaceFormat,
	#superclass : #PRFormat,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRMonospaceFormat >> accept: aVisitor [
	^ aVisitor visitMonospaceFormat: self
]
