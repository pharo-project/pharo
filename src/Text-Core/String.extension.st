Extension { #name : #String }

{ #category : #'*Text-Core' }
String >> asStringOrText [
	"Answer this string."

	^ self

]

{ #category : #'*Text-Core' }
String >> asText [
	"Answer a Text whose string is the receiver."

	^Text fromString: self
]

{ #category : #'*Text-Core' }
String >> encompassLine: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a cr (or eventually 1) and ends before a cr (or eventually at self size)
	See also encompassParagraph:"
	| left right |
	left := (self lastIndexOf: Character cr startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	right := (self indexOf: Character cr startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.
	^left to: right
]

{ #category : #'*Text-Core' }
String >> encompassParagraph: anInterval [
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a cr (or eventually 1) and ends at a cr (or eventually at self size).
	See also #encompassLine:"
	| left right |
	left := (self lastIndexOf: Character cr startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	right := (self indexOf: Character cr startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).
	^left to: right
]

{ #category : #'*Text-Core' }
String >> endOfParagraphBefore: aNumber [
	"Return the index of the last Character cr before position aNumber, or zero if this is the first paragraph.
	'ddd' endOfParagraphBefore: 3
	'dd
	d' endOfParagraphBefore: 4
	"
	^ self lastIndexOf: Character cr startingAt: aNumber - 1 ifAbsent: [ 0 ]
]
