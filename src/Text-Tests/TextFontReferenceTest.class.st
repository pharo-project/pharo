"
SUnit tests for TextFontReference
"
Class {
	#name : #TextFontReferenceTest,
	#superclass : #HashAndEqualsTestCase,
	#category : 'Text-Tests'
}

{ #category : #running }
TextFontReferenceTest >> setUp [
	super setUp.
	prototypes
		add: (TextFontReference
				toFont: (StrikeFont familyName: 'NewYork' size: 15)) 
]
