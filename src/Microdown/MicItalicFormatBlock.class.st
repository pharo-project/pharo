"
I represent an italic text section. I'm delimited using `_` as in `_italic_` to obtain _italic_.
"
Class {
	#name : #MicItalicFormatBlock,
	#superclass : #MicFormatBlock,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicItalicFormatBlock >> accept: aVisitor [
	^ aVisitor visitItalic: self
]

{ #category : #accessing }
MicItalicFormatBlock >> closingDelimiter [

 	^ MicItalicDelimiter markup
]

{ #category : #accessing }
MicItalicFormatBlock >> kind [

	^ #italic
]

{ #category : #accessing }
MicItalicFormatBlock >> openingDelimiter [

 	^ MicItalicDelimiter markup
]
