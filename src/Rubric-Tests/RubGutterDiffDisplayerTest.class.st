"
Tests for RubGutterDiffDisplayer - the gutter diff ruler.
"
Class {
	#name : 'RubGutterDiffDisplayerTest',
	#superclass : 'TestCase',
	#instVars : [
		'morph',
		'ruler'
	],
	#category : 'Rubric-Tests-Editing-Widgets',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Widgets'
}

{ #category : 'running' }
RubGutterDiffDisplayerTest >> baselineCode [

	^ 'line1
line2
line3'
]

{ #category : 'running' }
RubGutterDiffDisplayerTest >> setUp [

	super setUp.
	morph := RubScrolledTextMorph new.
	morph extent: 400 @ 250.
	morph withGutterDiff.
	ruler := morph gutterDiffRuler.
	ruler baseline: self baselineCode
]

{ #category : 'running' }
RubGutterDiffDisplayerTest >> tearDown [

	morph ifNotNil: [ morph delete ].
	super tearDown
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testAllLinesModified [

	| newSourceCode |
	newSourceCode := 'new1
new2
new3'.

	self testDiffWith: newSourceCode diff: #( #modified #modified #modified ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testAllLinesUnchanged [

	| newSourceCode |
	newSourceCode := 'line1
line2
line3'.

	self testDiffWith: newSourceCode diff: #( #unchanged #unchanged #unchanged ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testDiffFfrom: baselineCode With: newSourceCode diff: diff removed: removed [

	ruler baseline: baselineCode.
	self testDiffWith: newSourceCode diff: diff removed: removed
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testDiffWith: newSourceCode diff: diffArray removed: removedArray [

	morph setTextWith: newSourceCode.

	ruler computeLineStates.
	
	self assert: ruler lineStates equals: diffArray asArray.

	self assert: ruler removedPositions equals: removedArray asOrderedCollection
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testFirstLineModified [
	"Positional: position 1 differs → #modified (blue)"

	| newSourceCode |
	newSourceCode := 'modified1
line2
line3'.

	self testDiffWith: newSourceCode diff: #( #modified #unchanged #unchanged ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testGutterDiffRulerAdded [

	self assert: ruler notNil.
	self assert: ruler class equals: RubGutterDiffDisplayer
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testGutterDiffRulerRemoved [

	morph withoutGutterDiff.
	self assert: morph gutterDiffRuler isNil
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testInsertAndEditNearby [
	"Editing a line AND inserting a line nearby: patch-sequence walking groups the
	insert and adjacent modify into one modification block because both have no
	LCS match and are followed by a remove+match."

	| baselineCode newSourceCode |
	baselineCode := 'line1
line2
line3
line4'.

	newSourceCode := 'line1
inserted
edited2
line3
line4'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #modified #modified #unchanged #unchanged )
		removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testInsertLineInMiddle [
	"LCS-based: inserted line → #added (green), shifted lines remain #unchanged"

	| newSourceCode |
	newSourceCode := 'line1
inserted
line2
line3'.

	self testDiffWith: newSourceCode diff: #( #unchanged #added #unchanged #unchanged ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testLastLineModified [
	"Positional: position 3 differs → #modified (blue)"

	| newSourceCode |
	newSourceCode := 'line1
line2
modified3'.

	self testDiffWith: newSourceCode diff: #( #unchanged #unchanged #modified ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testLineStatesAfterTextChanged [

	| newSourceCode |
	newSourceCode := 'line1
line2
line3'.

	self testDiffWith: newSourceCode diff: #( #unchanged #unchanged #unchanged ) removed: {  }
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testLinesRemovedAtEnd [
	"Trailing baseline lines get removal arrows at their original positions"

	| newSourceCode |
	newSourceCode := 'line1'.

	self testDiffWith: newSourceCode diff: #( #unchanged ) removed: #( 2 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testMultipleDeletionsInMiddle [
	"Deleting multiple lines in the middle leaves remaining lines LCS-matched → #unchanged,
	with removal arrows at the original baseline positions"

	| baselineCode newSourceCode |
	baselineCode := 'a
b
c
d
e'.
	newSourceCode := 'a
d
e'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #unchanged #unchanged )
		removed: #( 2 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testMultipleLinesModified [
	"Positional: positions 1 and 3 differ → #modified (blue)"

	| newSourceCode |
	newSourceCode := 'modified1
line2
modified3'.

	self testDiffWith: newSourceCode diff: #( #modified #unchanged #modified ) removed: #(  )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testNonConsecutiveRemovalsKeepSeparateArrows [
	"Non-adjacent deletions each keep their own arrow (not collapsed).
	Each unmatched baseline line becomes a gap at the position in the current text
	where it would have appeared. Remaining current lines are #unchanged because
	they have direct LCS matches."

	| baselineCode newSourceCode |
	baselineCode := 'a
b
c
d
e'.
	newSourceCode := 'a
c
e'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #unchanged #unchanged )
		removed: #( 2 3 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testRemovedPositionInsideText [
	"Deleting middle line shows removal arrow at original position; remaining line is LCS-matched → #unchanged"

	| baselineCode newSourceCode |
	baselineCode := 'line1
line2
line3'.
	newSourceCode := 'line1
line3'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #unchanged )
		removed: #( 2 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testRemovedPositionsAfterDeletion [
	"Trailing removal records the original baseline line index"

	| baselineCode newSourceCode |
	baselineCode := 'a
b
c
d'.
	newSourceCode := 'a
b
c'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #unchanged #unchanged )
		removed: #( 4 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testRemovedPositionsMultipleDeletions [
	"Multiple trailing deletions record their original baseline indices"

	| baselineCode newSourceCode |
	baselineCode := 'a
b
c
d
e'.
	newSourceCode := 'a
b'.

	self
		testDiffFfrom: baselineCode
		With: newSourceCode
		diff: #( #unchanged #unchanged )
		removed: #( 3 )
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testWithGutterDiffConvenience [

	| morph2 ruler2 |
	morph2 := RubScrolledTextMorph new.
	morph2 withGutterDiff.
	ruler2 := morph2 gutterDiffRuler.

	self assert: ruler2 notNil.
	self assert: (morph2 hasRulerNamed: #gutterDiff).
	morph2 delete
]

{ #category : 'tests' }
RubGutterDiffDisplayerTest >> testWithoutGutterDiffConvenience [

	| morph2 |
	morph2 := RubScrolledTextMorph new.
	morph2 withGutterDiff.
	self assert: (morph2 hasRulerNamed: #gutterDiff).

	morph2 withoutGutterDiff.
	self deny: (morph2 hasRulerNamed: #gutterDiff).
	morph2 delete
]
