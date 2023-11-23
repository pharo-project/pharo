Extension { #name : 'WideString' }

{ #category : '*Monticello' }
WideString class >> newFromStream: aStream [
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."

	| len |
	aStream next = 16r80 ifTrue: [ "A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented' ].
	aStream skip: -1.
	len := aStream nextInt32.
	^ aStream nextWordsInto: (self basicNew: len)
]
