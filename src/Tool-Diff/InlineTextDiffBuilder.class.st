Class {
	#name : #InlineTextDiffBuilder,
	#superclass : #TextDiffBuilder,
	#category : #'Tool-Diff'
}

{ #category : #'as yet unclassified' }
InlineTextDiffBuilder >> split: aString [
	"Answer the split 'lines' by splitting on whitespace."
	
	|str lines sep|
	lines := OrderedCollection new.
	sep := Character separators, '^()[]{}''"`;.'.
	str := aString readStream.
	[str atEnd] whileFalse: [
		lines add: (str upToAny: sep).
		str atEnd ifFalse: [
			str skip: -1.
			lines add: str next asString]].
	^lines
]
