"
I'm a block starting with !!text paragraph (@@note @@important in Pillar)

!!note This is important so I used exclamatation mark.

For now I do not accept top level blocks.

1. one
1. Two

"
Class {
	#name : #MicAnnotatedBlock,
	#superclass : #MicAbstractAnnotatedBlock,
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicAnnotatedBlock >> addLineAndReturnNextNode: line [
	"line is assumed to be of the form '@@label some text
	
	'
	"

	isClosed
		ifTrue: [ ^ self ].
	label 
		ifNil: [ 
			| indexOfFirstSpace |
			indexOfFirstSpace := line indexOf: Character space.
			label := line copyFrom: self lineMarkup size + 1 to: indexOfFirstSpace - 1.
			body := (line copyFrom: indexOfFirstSpace + 1 to: line size) trim ]
		ifNotNil: [ body := body , String cr , line ]
]

{ #category : #testing }
MicAnnotatedBlock >> canConsumeLine: line [
	"return if this block can consume line"

	"it can be dangerous but let us try"
	^ line isNotEmpty 
]

{ #category : #accessing }
MicAnnotatedBlock >> label [
	^ label
]

{ #category : #testing }
MicAnnotatedBlock >> lineMarkup [

	^ parser annotatedParagraphMarkup
]

{ #category : #printing }
MicAnnotatedBlock >> printOn: aStream [

	aStream nextPutAll: 'Annotated: '.
	aStream nextPutAll: (self text asString contractTo: 100)
]

{ #category : #accessing }
MicAnnotatedBlock >> text [ 

	^ self body
]
