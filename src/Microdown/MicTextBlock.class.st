"
I'm a special inline block representing the fact that there is not markup. 
"
Class {
	#name : #MicTextBlock,
	#superclass : #MicInlineElement,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicTextBlock >> accept: aVisitor [
	^ aVisitor visitText: self
]

{ #category : #accessing }
MicTextBlock >> kind [

	^ #basic
]

{ #category : #printing }
MicTextBlock >> printOn: aStream [

	aStream nextPutAll: self substring.

]
