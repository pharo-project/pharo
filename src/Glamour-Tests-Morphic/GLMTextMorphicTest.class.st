Class {
	#name : #GLMTextMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #private }
GLMTextMorphicTest >> simulateOnlyOneClickOn: aMorph [
	(Delay forMilliseconds: HandMorph doubleClickTime + 1) wait.
	aMorph simulateClick
]

{ #category : #tests }
GLMTextMorphicTest >> testExplicitAcceptDoesNotAffectTextPort [
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123' ].
	window := composite openOn: 4.

	window position: 0 @ 0.
	window extent > Display extent ifTrue: [ "ignore test if the display size is too small and we can not simulate events properly" ^ self ].

	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	self simulateKeyStrokes: '4'.
	self assert: (composite pane port: #text) value equals: '1234'.
	self simulateKeyStroke: $s meta.
	self simulateKeyStrokes: '56'.
	self assert: (composite pane port: #text) value equals: '123456'
]

{ #category : #tests }
GLMTextMorphicTest >> testMultipleSelectionWithChange [
	| composite textMorph textPresentation |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self assertEmpty: textMorph selectionInterval.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	self assert: textMorph textArea editor selection asString equals: '2345'.
	self assert: textPresentation selectedText asString equals: '2345'
]

{ #category : #tests }
GLMTextMorphicTest >> testPastingUpdatesTextPort [
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 42.
	textMorph := self find: RubScrolledTextMorph in: window.
	Clipboard clipboardText: '42'.
	textMorph textArea editor paste.
	self assert: (composite pane port: #text) value equals: '42'
]

{ #category : #tests }
GLMTextMorphicTest >> testResettingTheSelection [
	| composite textPresentation |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	self shouldnt: [textPresentation selectionInterval: nil] raise: Error
]

{ #category : #tests }
GLMTextMorphicTest >> testTextPortPopulatedByDefault [
	| composite |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'
]

{ #category : #tests }
GLMTextMorphicTest >> testUnregisterFromAnnouncementsWhenUpdating [
	| composite announcingCollection numberOfExecutions |
	numberOfExecutions := 0.
	announcingCollection := GLMAnnouncingCollection new.
	composite :=  GLMCompositePresentation new with: [ :a | 
					(a text)
						updateOn: GLMItemAdded from: [ announcingCollection ];
						display: [ numberOfExecutions := numberOfExecutions + 1 ] ].
	window := composite openOn: announcingCollection.
	1 to: 3 do: [ :each | announcingCollection add: each ].
	self assert: numberOfExecutions equals: 4	"it should not be 8"
]

{ #category : #tests }
GLMTextMorphicTest >> testUpdatingTextPortUpdatesTheMorph [
	<expectedFailure>
	| composite textMorph |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: 'fourtytwo'.
	composite first text: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'.
	textMorph := self find: RubPluggableTextMorph in: window.
	self assert: textMorph text asString equals: '42'
]
