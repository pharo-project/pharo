"
I am a Canvas in the Canvas-Brush pattern. I keep an output stream of type `Text`.
As texts can have multiple annotations, I have a stack of brushes to paint a text with. 
My only method for adding text to my stream is `<<`.
"
Class {
	#name : #PRRichTextCanvas,
	#superclass : #Object,
	#instVars : [
		'out',
		'brushes',
		'nesting',
		'crAtEnd'
	],
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #public }
PRRichTextCanvas >> << aText [
	brushes do: [ :brush | brush paint: aText ].
	out << aText.
	aText ifNotEmpty: [crAtEnd := aText last = Character cr].
]

{ #category : #public }
PRRichTextCanvas >> contents [
	^ out contents
]

{ #category : #public }
PRRichTextCanvas >> cr [
	self newLine
]

{ #category : #public }
PRRichTextCanvas >> includeAttribute: attr in: aBlock [
	brushes addLast: (PRRichTextBrush on: self attribute: attr).
	aBlock value.
	brushes removeLast
]

{ #category : #public }
PRRichTextCanvas >> includeBrush: aBrush in: aBlock [
	"Add for the course of aBlock, a brush (aBrush) to the currently used brushes."
	brushes addLast: aBrush.
	aBlock value.
	brushes removeLast
]

{ #category : #public }
PRRichTextCanvas >> indentIn: aBlock [ 
	nesting := nesting +1.
	aBlock value.
	nesting := nesting -1.
	
		
]

{ #category : #initialization }
PRRichTextCanvas >> initialize [
	super initialize.
	out := WriteStream on: '' asText.
	brushes := OrderedCollection with: (PRRichTextIndentBrush on: self).
	nesting := 0.
	crAtEnd := true. "Prevent newlines in beginning"
]

{ #category : #accessing }
PRRichTextCanvas >> nesting [
	^ nesting
]

{ #category : #public }
PRRichTextCanvas >> newLine [
	self newLineBody.
	crAtEnd := true
]

{ #category : #private }
PRRichTextCanvas >> newLineBody [
	self << String cr asText
]

{ #category : #public }
PRRichTextCanvas >> newLineIfNotAlready [
	crAtEnd
		ifFalse: [ self newLineBody ].
	crAtEnd := true
]
