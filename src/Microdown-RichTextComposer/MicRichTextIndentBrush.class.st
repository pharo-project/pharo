"
I am a Brush that paints the `TextIndent` on a `Text`. The actual value of the indent is picked up from the canvas. 
"
Class {
	#name : #MicRichTextIndentBrush,
	#superclass : #MicRichTextBrush,
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #paint }
MicRichTextIndentBrush >> paint: aText [	
	canvas nesting isZero ifFalse: [  
		aText addAttribute: (TextIndent tabs: canvas nesting) ]
]
