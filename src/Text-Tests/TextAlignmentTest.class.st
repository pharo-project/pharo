"
SUnit tests for class TextAlignment
"
Class {
	#name : #TextAlignmentTest,
	#superclass : #HashAndEqualsTestCase,
	#category : #'Text-Tests-Base'
}

{ #category : #running }
TextAlignmentTest >> setUp [
	super setUp.
	prototypes add: TextAlignment centered;
		 add: TextAlignment justified;
		 add: TextAlignment leftFlush;
		 add: TextAlignment rightFlush 
]

{ #category : #tests }
TextAlignmentTest >> testAlignment [
	| attribute |
	self assert: TextAlignment leftFlush alignment equals: TextConstants LeftFlush. 
	self assert: TextAlignment rightFlush alignment equals: TextConstants RightFlush. 
	self assert: TextAlignment centered alignment equals: TextConstants Centered . 
	self assert: TextAlignment justified  alignment equals: TextConstants Justified . 
	"We can change alignment"
	attribute := TextAlignment leftFlush.
	attribute alignment: TextConstants RightFlush.
	self assert: attribute alignment equals: TextConstants RightFlush 
]

{ #category : #tests }
TextAlignmentTest >> testDominates [
	| text |
	self assert: (TextAlignment centered dominates: TextAlignment rightFlush).
	self assert: (TextAlignment centered  dominates: TextAlignment centered).
	self deny: (TextAlignment centered dominates: TextEmphasis bold).
	self deny: (TextAlignment centered dominates: TextFontChange font2).
	"In action"
	text := 'Pharo is cool' asText.
	text addAttribute: TextAlignment centered.
	"Only one alignement  time, the last added alignement win"
	text addAttribute: TextAlignment justified .
	self assert: (text attributesAt: 1) size equals: 1.
	self assert: (text attributesAt: 1) first equals: TextAlignment justified.
	"Can be both aligned and bold"
	text addAttribute: TextEmphasis bold.
	self assert: (text attributesAt: 1) size equals: 2.
	self assert: (text attributesAt: 1) first  equals: TextAlignment justified.
	self assert: (text attributesAt: 2) second emphasisCode equals: 1
]
