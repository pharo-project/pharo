Class {
	#name : #MicFootnoteBlock,
	#superclass : #MicAnnotationBlock,
	#category : #'Microdown-Extensions'
}

{ #category : #accessing }
MicFootnoteBlock class >> tag [

	^ #footnote
]

{ #category : #visiting }
MicFootnoteBlock >> accept: aVisitor [
	aVisitor visitFootnote: self
]

{ #category : #accessing }
MicFootnoteBlock >> note [
	arguments ifNotEmpty: [ 
		arguments at: #note ifAbsent: [ ^ '' ].
		^ arguments at: #note ]
]
