Class {
	#name : #TextColorTest,
	#superclass : #TestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #tests }
TextColorTest >> testColor [
	| textColor |
	textColor := TextColor color: Color red.
	self assert: textColor color equals: Color red.
	self assert: textColor asColor equals: Color red.
	
	textColor  color: Color yellow.
	self assert: textColor color equals: Color yellow.
	self assert: textColor asColor equals: Color yellow.
	
]

{ #category : #tests }
TextColorTest >> testDominates [
	| text |
	self assert: (TextColor red dominates: TextColor yellow).
	self assert: (TextColor red dominates: TextColor red).
	self deny: (TextColor red dominates: TextEmphasis bold).
	self deny: (TextColor red dominates: TextFontChange font2).
	"In action"
	text := 'Pharo is cool' asText.
	text addAttribute: TextColor red.
	"Only one color at time, the last added color win"
	text addAttribute: TextColor yellow.
	self assert: (text attributesAt: 1) size equals: 1.
	self assert: (text attributesAt: 1) first color equals: Color yellow.
	"Can be both colored and bold"
	text addAttribute: TextEmphasis bold.
	self assert: (text attributesAt: 1) size equals: 2.
	self assert: (text attributesAt: 1) first color equals: Color yellow.
	self assert: (text attributesAt: 2) second emphasisCode equals: 1
]
