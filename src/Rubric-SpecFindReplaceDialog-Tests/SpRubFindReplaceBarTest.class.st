"
I test the inline find and replace bar (RubFindReplaceBar) as the IDE uses it: in the editor of a SpCodePresenter, with the find and replace service installed by the IDE (SpRubFindReplaceService).
"
Class {
	#name : 'SpRubFindReplaceBarTest',
	#superclass : 'TestCase',
	#instVars : [
		'presenter',
		'window',
		'previousServiceClass'
	],
	#category : 'Rubric-SpecFindReplaceDialog-Tests',
	#package : 'Rubric-SpecFindReplaceDialog-Tests'
}

{ #category : 'accessing' }
SpRubFindReplaceBarTest >> bar [

	^ self scrolledText rulerNamed: #findReplaceBar
]

{ #category : 'helpers' }
SpRubFindReplaceBarTest >> keyDown: aKeyboardKey shift: aBoolean in: aField [
	"Send a key down event to aField, as the keyboard does"

	| event |
	event := KeyboardEvent new
		         setType: #keyDown
		         buttons: (aBoolean ifTrue: [ 8 ] ifFalse: [ 0 ])
		         position: 0 @ 0
		         keyValue: 0
		         charCode: 0
		         hand: nil
		         stamp: 0.
	event key: aKeyboardKey.
	aField textArea keyDown: event
]

{ #category : 'helpers' }
SpRubFindReplaceBarTest >> openFind [
	"What Cmd+F does in the editor"

	self textArea editor find.
	^ self bar
]

{ #category : 'accessing' }
SpRubFindReplaceBarTest >> scrolledText [

	^ self textArea ownerThatIsA: RubScrolledTextMorph
]

{ #category : 'accessing' }
SpRubFindReplaceBarTest >> service [

	^ self textArea findReplaceService
]

{ #category : 'running' }
SpRubFindReplaceBarTest >> setUp [

	super setUp.
	previousServiceClass := RubEditingArea defaultFindReplaceServiceClass.
	RubEditingArea defaultFindReplaceServiceClass: SpRubFindReplaceService.
	presenter := SpCodePresenter new.
	presenter text: 'foo bar foo
baz foo qux'.
	window := presenter open.
	self textArea takeKeyboardFocus.
	"The service follows the focused text area"
	self service textArea: self textArea.
	self textArea selectFrom: 1 to: 0
]

{ #category : 'running' }
SpRubFindReplaceBarTest >> tearDown [

	window ifNotNil: [ window close ].
	RubEditingArea defaultFindReplaceServiceClass: previousServiceClass.
	super tearDown
]

{ #category : 'accessing' }
SpRubFindReplaceBarTest >> textArea [

	"The editor of the code presenter (the fields of the find bar are text areas too)"

	^ presenter adapter widget textArea
]

{ #category : 'helpers' }
SpRubFindReplaceBarTest >> type: aString [

	self bar findField setText: aString.
	self bar findFieldChanged
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testCloseGivesTheFocusBackToTheEditor [

	self openFind.
	self service closeFindBar.
	self assert: self bar isNil.
	self assert: self textArea hasKeyboardFocus
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testCloseKeepsTheServiceListeningToTheEditor [
	"Closing the bar must only drop its own subscription, not the ones of the editor to its service"

	| subscribedAnnouncements |
	self openFind.
	self service closeFindBar.
	subscribedAnnouncements := (self textArea announcer subscriptions subscriptions select: [ :each | each subscriber == self service ])
		                           collect: [ :each | each announcementClass ].
	self assert: (subscribedAnnouncements includesAll: { MorphGotFocus. RubSelectionChanged. RubFindReplaceWindowRequired }).
	self deny: (subscribedAnnouncements includes: RubTextChanged).
	self openFind.
	self assert: self bar isNotNil
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testCmdFAgainKeepsASingleBar [

	| bar |
	bar := self openFind.
	self openFind.
	self assert: self bar identicalTo: bar.
	self assert: (self scrolledText rulers select: [ :each | each key = #findReplaceBar ]) size equals: 1
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testCountUpdatesWhileTyping [

	self openFind.
	self type: 'fo'.
	self assert: self bar countString equals: '1 of 3'.
	self type: 'bar'.
	self assert: self bar countString equals: '1 of 1'.
	self type: 'nothing'.
	self assert: self bar countString equals: 'No results'.
	self type: ''.
	self assert: self bar countString equals: ''
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testEnterAndShiftEnterNavigate [

	self openFind.
	self type: 'foo'.
	self assert: self textArea selectionInterval equals: (1 to: 3).
	self bar findNext.
	self assert: self bar countString equals: '2 of 3'.
	self bar findNext.
	self bar findNext.
	self assert: self bar countString equals: '1 of 3'.
	self bar findPrevious.
	self assert: self bar countString equals: '3 of 3'.
	self assert: self textArea selectionInterval equals: (17 to: 19)
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testEnterShiftEnterAndEscapeKeysInTheFindField [

	| bar |
	bar := self openFind.
	self type: 'foo'.
	self keyDown: KeyboardKey enter shift: false in: bar findField.
	self assert: bar countString equals: '2 of 3'.
	self keyDown: KeyboardKey enter shift: true in: bar findField.
	self keyDown: KeyboardKey enter shift: true in: bar findField.
	self assert: bar countString equals: '3 of 3'.
	self assert: bar findText equals: 'foo'.
	self keyDown: KeyboardKey escape shift: false in: bar findField.
	self assert: self bar isNil.
	self assert: self textArea selectionInterval equals: (17 to: 19)
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testEscapeInTheEditorClosesTheBar [

	self openFind.
	self textArea escapePressed.
	self assert: self bar isNil
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testFindIsSeededWithTheSelection [

	self textArea selectFrom: 5 to: 7.
	self openFind.
	self assert: self bar findText equals: 'bar'.
	self assert: self bar countString equals: '1 of 1'
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testMatchesAreOnlyComputedWhileTheBarIsOpen [

	self service findText: 'foo'.
	self scrolledText imageForm.
	self deny: self service hasActiveSearch.
	self assert: (self service instVarNamed: #matchRanges) isNil.
	self openFind.
	self assert: self service hasActiveSearch.
	self service closeFindBar.
	self deny: self service hasActiveSearch
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testOpensInlineInTheFocusedEditor [

	| windowsBefore bar |
	windowsBefore := self currentWorld submorphs size.
	bar := self openFind.
	self assert: (bar isKindOf: RubFindReplaceBar).
	self assert: bar owner identicalTo: self scrolledText.
	self assert: self currentWorld submorphs size equals: windowsBefore.
	self assert: (self scrolledText bounds containsRect: bar bounds).
	self assert: bar findField textArea hasKeyboardFocus
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testReplaceAndReplaceAllKeepTheCount [

	self openFind.
	self type: 'foo'.
	self bar replaceField setText: 'x'.
	self bar replace.
	self assert: self textArea text asString equals: 'x bar foo
baz foo qux'.
	self assert: self bar countString equals: '1 of 2'.
	self bar replaceAll.
	self assert: self textArea text asString equals: 'x bar x
baz x qux'.
	self assert: self bar countString equals: 'No results'
]

{ #category : 'tests' }
SpRubFindReplaceBarTest >> testTogglesChangeTheSearch [

	self openFind.
	self type: 'Foo'.
	self assert: self bar countString equals: '1 of 3'.
	self bar caseButton press.
	self assert: self service caseSensitive.
	self assert: self bar caseButton isSelected.
	self assert: self bar countString equals: 'No results'.
	self bar caseButton press.
	self type: 'ba.'.
	self bar regexButton press.
	self assert: self bar countString equals: '? of 2'
]
