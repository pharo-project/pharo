"
SUnit tests for TextFontReference
"
Class {
	#name : #TextFontReferenceTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextFontReferenceTest >> setUp [
	super setUp.
	prototypes
		add: (TextFontReference
				toFont: (StrikeFont familyName: 'NewYork' size: 15)) 
]

{ #category : #accessing }
TextFontReferenceTest >> testFont [
	| font text |
	font := LogicalFont familyName: 'Source Sans Pro' pointSize: 8.
	text := 'Hello' asText addAttribute: (TextFontReference toFont: font).
	"Querry the font in this text"
	self assert: (text runs at: 1) first font equals: font
]
