"
I test the find and replace engine of FindReplaceService: counting the matches, navigating between them with wrap around, and replacing them.
"
Class {
	#name : 'RubFindReplaceServiceTest',
	#superclass : 'TestCase',
	#instVars : [
		'scrolledText',
		'service'
	],
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'helpers' }
RubFindReplaceServiceTest >> caretAt: anIndex [

	self textArea selectFrom: anIndex to: anIndex - 1
]

{ #category : 'helpers' }
RubFindReplaceServiceTest >> select: anInterval [

	self textArea selectFrom: anInterval first to: anInterval last
]

{ #category : 'helpers' }
RubFindReplaceServiceTest >> selection [

	^ self textArea selectionInterval
]

{ #category : 'running' }
RubFindReplaceServiceTest >> setText: aString [

	scrolledText setText: aString.
	self caretAt: 1
]

{ #category : 'running' }
RubFindReplaceServiceTest >> setUp [

	super setUp.
	scrolledText := RubScrolledTextMorph new.
	scrolledText extent: 400 @ 200.
	service := FindReplaceService new.
	service textArea: scrolledText textArea
]

{ #category : 'running' }
RubFindReplaceServiceTest >> tearDown [

	scrolledText delete.
	super tearDown
]

{ #category : 'helpers' }
RubFindReplaceServiceTest >> text [

	^ self textArea text asString
]

{ #category : 'helpers' }
RubFindReplaceServiceTest >> textArea [

	^ scrolledText textArea
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountIsCappedOnHugeTexts [

	self setText: (String new: 4000 withAll: $a).
	service findText: 'a'.
	self assert: service matchRanges size equals: FindReplaceService maxMatchCount.
	self assert: service matchesTruncated
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountIsExactBelowTheCap [

	self setText: (String new: FindReplaceService maxMatchCount withAll: $a).
	service findText: 'a'.
	self assert: service matchRanges size equals: FindReplaceService maxMatchCount.
	self deny: service matchesTruncated
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountIsInvalidatedWhenTheTextChanges [

	self setText: 'foo foo'.
	service findText: 'foo'.
	self assert: service matchRanges size equals: 2.
	self caretAt: 8.
	self textArea replaceSelectionWith: ' foo'.
	self assert: service matchRanges size equals: 3
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountPlainIgnoringCase [

	self setText: 'foo Foo FOO bar'.
	service findText: 'foo'.
	self assert: service matchRanges asArray equals: { 1 to: 3. 5 to: 7. 9 to: 11 }
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountPlainMatchingCase [

	self setText: 'foo Foo FOO bar'.
	service caseSensitive: true.
	service findText: 'Foo'.
	self assert: service matchRanges asArray equals: { 5 to: 7 }
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountRegex [

	self setText: 'a1 b22 c333'.
	service isRegex: true.
	service findText: '\d+'.
	self assert: service matchRanges asArray equals: { 2 to: 2. 5 to: 6. 9 to: 11 }
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountSpecialCharactersArePlainText [

	self setText: 'a.b axb a.b'.
	service findText: 'a.b'.
	self assert: service matchRanges asArray equals: { 1 to: 3. 9 to: 11 }
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testCountWholeWords [

	self setText: 'foo foobar xfoo foo_ foo'.
	service entireWordsOnly: true.
	service findText: 'foo'.
	self assert: service matchRanges asArray equals: { 1 to: 3. 22 to: 24 }
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testEmptyFindTextHasNoMatches [

	self setText: 'foo'.
	service findText: ''.
	self assert: service matchRanges isEmpty.
	self assert: service findNext isNil
]

{ #category : 'tests - counting' }
RubFindReplaceServiceTest >> testInvalidRegexHasNoMatches [
	"Regression: an invalid regex used to be replaced by an empty regex, which matched an empty string everywhere"

	self setText: 'foo(bar'.
	service isRegex: true.
	service findText: 'foo('.
	self assert: service hasInvalidRegex.
	self assert: service matchRanges isEmpty.
	self assert: service findNext isNil
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testCurrentMatchIndex [

	self setText: 'foo bar foo'.
	service findText: 'foo'.
	self assert: service currentMatchIndex equals: 0.
	self select: (9 to: 11).
	self assert: service currentMatchIndex equals: 2.
	self select: (9 to: 10).
	self assert: service currentMatchIndex equals: 0
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testFindAgainUsesTheSearchOptions [
	"Regression: Cmd+G used a plain case sensitive substring search that ignored the options of the find bar"

	| previousClass |
	previousClass := RubEditingArea defaultFindReplaceServiceClass.
	RubEditingArea defaultFindReplaceServiceClass: FindReplaceService.
	[
	| editor area |
	editor := RubScrolledTextMorph new.
	area := editor textArea.
	editor setText: 'foobar Foo'.
	area findReplaceService textArea: area.
	area findReplaceService entireWordsOnly: true.
	area findReplaceService findText: 'foo'.
	area selectFrom: 1 to: 0.
	self assert: area editor findAgain equals: (8 to: 10).
	self assert: area selectionInterval equals: (8 to: 10) ] ensure: [
		RubEditingArea defaultFindReplaceServiceClass: previousClass ]
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testFindInDirection [

	self setText: 'foo foo foo'.
	service findText: 'foo'.
	self caretAt: 6.
	self assert: service find equals: (9 to: 11).
	service searchBackwards: true.
	self assert: service find equals: (5 to: 7)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextAdvancesFromTheCurrentMatch [

	self setText: 'a foo b foo c foo'.
	service findText: 'foo'.
	self assert: service findNext equals: (3 to: 5).
	self assert: service findNext equals: (9 to: 11).
	self assert: service findNext equals: (15 to: 17).
	self assert: self selection equals: (15 to: 17)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextFromTheCaret [

	self setText: 'foo bar foo baz foo'.
	service findText: 'foo'.
	self caretAt: 6.
	self assert: service findNext equals: (9 to: 11).
	self assert: service currentMatchIndex equals: 2
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextGoesPastTheCountCap [

	| cap |
	cap := FindReplaceService maxMatchCount.
	self setText: (String new: cap + 10 withAll: $a).
	service findText: 'a'.
	self select: (cap to: cap).
	self assert: service findNext equals: (cap + 1 to: cap + 1).
	self assert: service findNext equals: (cap + 2 to: cap + 2)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextWithoutMatchFlashesAndKeepsTheSelection [

	self setText: 'foo bar'.
	service findText: 'baz'.
	self select: (1 to: 3).
	self assert: service findNext isNil.
	self assert: self selection equals: (1 to: 3)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextWithoutWrapAroundStopsAtTheEnd [

	self setText: 'foo foo'.
	service wrapAround: false.
	service findText: 'foo'.
	self select: (5 to: 7).
	self assert: service findNext isNil.
	self assert: self selection equals: (5 to: 7)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testNextWrapsAround [

	self setText: 'foo bar foo'.
	service findText: 'foo'.
	self select: (9 to: 11).
	self assert: service findNext equals: (1 to: 3).
	self assert: service currentMatchIndex equals: 1
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testPreviousFromTheCaret [

	self setText: 'foo bar foo baz foo'.
	service findText: 'foo'.
	self caretAt: 14.
	self assert: service findPrevious equals: (9 to: 11)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testPreviousGoesBackFromTheCurrentMatch [

	self setText: 'foo bar foo baz foo'.
	service findText: 'foo'.
	self select: (17 to: 19).
	self assert: service findPrevious equals: (9 to: 11).
	self assert: service findPrevious equals: (1 to: 3)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testPreviousWrapsAround [

	self setText: 'foo bar foo'.
	service findText: 'foo'.
	self select: (1 to: 3).
	self assert: service findPrevious equals: (9 to: 11)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testWholeWordFromInsideAWord [
	"Regression: the search started on a copy of the text that began at the caret, so the end of a word looked like a whole word"

	self setText: 'xfoo foo'.
	service entireWordsOnly: true.
	service findText: 'foo'.
	self caretAt: 2.
	self assert: service findNext equals: (6 to: 8)
]

{ #category : 'tests - navigation' }
RubFindReplaceServiceTest >> testWholeWordRegexFromInsideAWord [

	self setText: 'xfoo foo'.
	service isRegex: true.
	service entireWordsOnly: true.
	service findText: 'fo+'.
	self caretAt: 2.
	self assert: service findNext equals: (6 to: 8)
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceAll [

	self setText: 'foo x Foo y foo'.
	service findText: 'foo'.
	service replaceText: 'bar'.
	self assert: service replaceAll equals: 3.
	self assert: self text equals: 'bar x bar y bar'.
	self assert: service matchRanges isEmpty
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceAllIsASingleUndo [

	self setText: 'foo x foo'.
	service findText: 'foo'.
	service replaceText: 'bar'.
	service replaceAll.
	self textArea editor undo.
	self assert: self text equals: 'foo x foo'
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceAllWithoutMatch [

	self setText: 'foo'.
	service findText: 'baz'.
	self assert: service replaceAll equals: 0.
	self assert: self text equals: 'foo'
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceKeepsCountAndPosition [

	self setText: 'foo foo foo'.
	service findText: 'foo'.
	service replaceText: 'x'.
	service findNext.
	self assert: service replace equals: (3 to: 5).
	self assert: self text equals: 'x foo foo'.
	self assert: service matchRanges size equals: 2.
	self assert: service currentMatchIndex equals: 1
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceTheLastMatchWrapsAround [

	self setText: 'foo bar foo'.
	service findText: 'foo'.
	service replaceText: 'baz'.
	self select: (9 to: 11).
	self assert: service replace equals: (1 to: 3).
	self assert: self text equals: 'foo bar baz'
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceWhenTheReplacementContainsTheMatch [

	self setText: 'a b a'.
	service findText: 'a'.
	service replaceText: 'aa'.
	service findNext.
	self assert: service replace equals: (6 to: 6).
	self assert: self text equals: 'aa b a'.
	self assert: service replace equals: (1 to: 1).
	self assert: self text equals: 'aa b aa'
]

{ #category : 'tests - replacing' }
RubFindReplaceServiceTest >> testReplaceWithoutSelectedMatchOnlySelectsIt [
	"Regression: replace used to replace the next match before showing it"

	self setText: 'foo foo'.
	service findText: 'foo'.
	service replaceText: 'x'.
	self assert: service replace equals: (1 to: 3).
	self assert: self text equals: 'foo foo'.
	self assert: service replace equals: (3 to: 5).
	self assert: self text equals: 'x foo'
]
