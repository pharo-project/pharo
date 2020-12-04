"
I am a Brush that paints the `TextIndent` on a `Text`. The actual value of the indent is picked up from the canvas. 
"
Class {
	#name : #PRRichTextIndentBrush,
	#superclass : #PRRichTextBrush,
	#category : #'Pillar-ExporterRichText-Core'
}

{ #category : #paint }
PRRichTextIndentBrush >> paint: aText [	
	canvas nesting isZero ifFalse: [  
		aText addAttribute: (TextIndent tabs: canvas nesting) ]
]
