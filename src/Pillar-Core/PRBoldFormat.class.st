"
In Pillar, text can be shown in bold by wrapping the text around `""""`.

`""""text in bold""""` renders as **text in bold**.

"
Class {
	#name : #PRBoldFormat,
	#superclass : #PRFormat,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRBoldFormat >> accept: aVisitor [
	^ aVisitor visitBoldFormat: self
]
