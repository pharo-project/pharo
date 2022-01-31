"
I represent preformatted paragraph indicated by '> ' at the beginning of each my lines. 
I'm like a preformatted in HTML or Pillar '= '.
Every single line should starts with '> '.

The semantics of microdown quote block is that blocks (code block, header, lists) are not interpreted inside only formatting.

In githubmarkdown the definitions are just super complex and we do not follow them. 
	""A block quote marker consists of 0-3 spaces of initial indent, plus (a) the character > together with a following space, or (b) a single character > not followed by a space.""


"
Class {
	#name : #MicQuoteBlock,
	#superclass : #MicContinuousMarkedBlock,
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicQuoteBlock >> accept: aVisitor [
 	^ aVisitor visitQuote: self
]

{ #category : #visiting }
MicQuoteBlock >> closeMe [
	super closeMe.
	
	children := MicInlineParser new parse: text
]

{ #category : #testing }
MicQuoteBlock >> lineMarkup [

	^ self parser preformattedMarkup
]
