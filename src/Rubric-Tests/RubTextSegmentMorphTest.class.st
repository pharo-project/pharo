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
	"
'foo
	fooBarZork
brain'"
	| textSegmentMorph vertices |
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
	vertices := textSegmentMorph getVertices asArray. 
	self assert: vertices equals: 
		{(8.796005249023438@25).
		(8.796005249023438@6).
		(24.888015747070313@6).
		(24.888015747070313@25).
		(74.70603942871094@25).
		(74.70603942871094@44).
		(18.70001220703125@44).
		(18.70001220703125@63).
		(5@63).
		(5@25)}.
	
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
