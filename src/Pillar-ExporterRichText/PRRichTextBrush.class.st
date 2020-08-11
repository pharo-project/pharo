"
I am a brush in the canvas-brush pattern.

I represent one kind of `TextAttribute` to be added to a `Text`.
"
Class {
	#name : #PRRichTextBrush,
	#superclass : #Object,
	#instVars : [
		'canvas',
		'attribute'
	],
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #'instance creation' }
PRRichTextBrush class >> on: canvas [ 
	^self new
		canvas: canvas
]

{ #category : #'instance creation' }
PRRichTextBrush class >> on: canvas attribute: attr [
	^self new
		canvas: canvas;
		attribute: attr
]

{ #category : #accessing }
PRRichTextBrush >> attribute [
	^ attribute
]

{ #category : #accessing }
PRRichTextBrush >> attribute: anObject [
	attribute := anObject
]

{ #category : #accessing }
PRRichTextBrush >> canvas [
	^ canvas
]

{ #category : #accessing }
PRRichTextBrush >> canvas: anObject [
	canvas := anObject
]

{ #category : #paint }
PRRichTextBrush >> paint: aText [
	aText addAttribute: attribute 
]
