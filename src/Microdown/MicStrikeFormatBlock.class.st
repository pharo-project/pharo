"
I represent a strike text section. I'm delimited using `~` as in `~strike~` to obtain ~strike~.
"
Class {
	#name : #MicStrikeFormatBlock,
	#superclass : #MicFormatBlock,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicStrikeFormatBlock >> accept: aVisitor [
	^ aVisitor visitStrike: self
]

{ #category : #accessing }
MicStrikeFormatBlock >> closingDelimiter [

 	^ MicStrikeDelimiter markup
]

{ #category : #accessing }
MicStrikeFormatBlock >> kind [ 
	^ #strike
]

{ #category : #accessing }
MicStrikeFormatBlock >> openingDelimiter [

 	^ MicStrikeDelimiter markup
]
