"
I am a brush in the canvas-brush pattern.

I represent one kind of `TextAttribute` to be added to a `Text`.
"
Class {
	#name : #MicRichTextBrush,
	#superclass : #Object,
	#instVars : [
		'canvas',
		'attribute'
	],
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #'instance creation' }
MicRichTextBrush class >> on: canvas [ 
	^self new
		canvas: canvas
]

{ #category : #'instance creation' }
MicRichTextBrush class >> on: canvas attribute: attr [
	^self new
		canvas: canvas;
		attribute: attr
]

{ #category : #accessing }
MicRichTextBrush >> attribute [
	^ attribute
]

{ #category : #accessing }
MicRichTextBrush >> attribute: anObject [
	attribute := anObject
]

{ #category : #accessing }
MicRichTextBrush >> canvas [
	^ canvas
]

{ #category : #accessing }
MicRichTextBrush >> canvas: anObject [
	canvas := anObject
]

{ #category : #paint }
MicRichTextBrush >> paint: aText [
	aText addAttribute: attribute 
]
