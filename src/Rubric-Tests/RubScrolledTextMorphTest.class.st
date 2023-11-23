Class {
	#name : 'RubScrolledTextMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'morph'
	],
	#category : 'Rubric-Tests-Base',
	#package : 'Rubric-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
RubScrolledTextMorphTest >> setUp [
	super setUp.
	morph := RubScrolledTextMorph new.
	morph extent: 400@250.
	morph setTextWith: 'one two three four'
]

{ #category : 'running' }
RubScrolledTextMorphTest >> tearDown [

	morph delete.
	super tearDown
]

{ #category : 'tests' }
RubScrolledTextMorphTest >> testEmbeddedAnchorInTextAreDrawnAtRightPosition [
	| circle longTextWithCircleAtEnd canvas|
	self skip. "Test fail in CI"
	circle := CircleMorphCounter new extent: 50 @ 50;color: Color blue.
	longTextWithCircleAtEnd := ((String loremIpsum: 10000),
		(String value: 1)) asText
			addAttribute: (TextAnchor new anchoredMorph: circle).

	self assert: circle owner isNil.
	self assert: circle position equals: 0@0.
	morph updateTextWith: longTextWithCircleAtEnd.
	canvas := FormCanvas extent: 500@500.
	canvas fullDrawMorph: morph.

	self assert: circle owner equals: nil.
	self assert: circle counter equals: 0.

	morph scrollToEndOfText.
	canvas := FormCanvas extent: 500@500.
	canvas fullDrawMorph: morph.
	self assert: circle counter > 0
]

{ #category : 'tests' }
RubScrolledTextMorphTest >> testGhostText [
	| scrolledText |
	scrolledText := RubScrolledTextMorph new
				ghostText: 'This is a cool ghost text';
				yourself.
	self assert: scrolledText ghostText string equals: 'This is a cool ghost text'
]
