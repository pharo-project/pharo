"
I represent a bold text section. I'm delimited using `**` as in `**bold**` to obtain **bold**.
"
Class {
	#name : #MicBoldFormatBlock,
	#superclass : #MicFormatBlock,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicBoldFormatBlock >> accept: aVisitor [
	^ aVisitor visitBold: self
]

{ #category : #utilcaca }
MicBoldFormatBlock >> closingDelimiter [

 	^ MicBoldDelimiter markup
]

{ #category : #utilcaca }
MicBoldFormatBlock >> kind [
	^ #bold
]

{ #category : #utilcaca }
MicBoldFormatBlock >> openingDelimiter [

 	^ MicBoldDelimiter markup
]
