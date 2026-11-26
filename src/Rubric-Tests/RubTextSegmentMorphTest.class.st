Class {
	#name : 'RubTextSegmentMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'editor',
		'textArea'
	],
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'running' }
RubTextSegmentMorphTest >> setUp [
	super setUp.
	textArea := RubEditingArea new.
	editor := RubTextEditor forTextArea: textArea.
	"Add text with a paragraph"
	editor addString: 'Lorem ipsum dolor sit amet'.
]

{ #category : 'running' }
RubTextSegmentMorphTest >> testBasic [
	| textSegmentMorph |
	textSegmentMorph := textArea newPrimarySelection.
	self assert: textSegmentMorph textArea equals: textArea.
	self assert: textSegmentMorph firstIndex equals: textArea markIndex.
	self assert: textSegmentMorph lastIndex equals: textArea pointIndex.
]

{ #category : 'running' }
RubTextSegmentMorphTest >> testComputeVerticesMultipleLines [
	"The selection covers three lines: 'oo' at the end of the first one, the whole second one, and 'br' at the start of the third one.
	The expected points come from the font, so the test checks the outline's shape with any font"

	| textSegmentMorph font gap left top lineHeight afterF endOfFirstLine endOfSecondLine afterBr |
	editor addString: 'foo
	BarZork
brain'.
	editor selectInterval: (2 to: 15).
	textSegmentMorph := textArea newPrimarySelection.
	self assert: textSegmentMorph firstIndex equals: 2.
	self assert: textSegmentMorph lastIndex equals: 16.
	self assert: editor selection equals: 'oo
	BarZork
br'.

	"The font the text is laid out with"
	font := textArea text fontAt: 1 withStyle: textArea textStyle.
	"The outline sits one pixel away from the selected characters"
	gap := 1 @ 0.
	left := textArea left + textArea margins left.
	top := textArea top + textArea margins top.
	lineHeight := textSegmentMorph lines first lineHeight.
	"Text is laid out with the font's unrounded widths, so add them up instead of asking widthOfString:, which rounds"
	afterF := left + (self widthOf: 'f' in: font).
	endOfFirstLine := left + (self widthOf: 'foo' in: font).
	"The second line starts with a tab"
	endOfSecondLine := left + textArea tabWidth + (self widthOf: 'BarZork' in: font).
	afterBr := left + (self widthOf: 'br' in: font).

	self assert: textSegmentMorph getVertices asArray equals: {
		(afterF @ (top + lineHeight) - gap).
		(afterF @ top - gap).
		"Along the right end of the first line, then of the second line"
		(endOfFirstLine @ top + gap).
		(endOfFirstLine @ (top + lineHeight) + gap).
		(endOfSecondLine @ (top + lineHeight) + gap).
		(endOfSecondLine @ (top + (2 * lineHeight)) + gap).
		"Down after the 'br' on the third line"
		(afterBr @ (top + (2 * lineHeight)) + gap).
		(afterBr @ (top + (3 * lineHeight)) + gap).
		"Back along the left margin, to the bottom of the first line"
		(left @ (top + (3 * lineHeight)) - gap).
		(left @ (top + lineHeight) - gap) }
	"From after the 'f', up to the top of the first line"
]

{ #category : 'running' }
RubTextSegmentMorphTest >> testComputeVerticesSingleLine [
	| textSegmentMorph vertices |
	"'Lorem ipsum dolor sit amet'"
	editor selectInterval: (1 to: 5)."Lorem"
	textSegmentMorph := textArea newPrimarySelection.
	self assert: textSegmentMorph firstIndex equals: 1.
	self assert: textSegmentMorph lastIndex equals: 6.
	self assert: editor selection equals: 'Lorem'.
	vertices := textSegmentMorph getVertices. 
	self assert: vertices size equals: 5.
	
]

{ #category : 'helpers' }
RubTextSegmentMorphTest >> widthOf: aString in: aFont [
	"The width of aString as the text is laid out: the sum of the characters' unrounded widths"

	^ aString inject: 0 into: [ :width :character | width + (aFont linearWidthOf: character) ]
]
