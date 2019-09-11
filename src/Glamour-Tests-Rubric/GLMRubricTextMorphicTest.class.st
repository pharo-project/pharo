Class {
	#name : #GLMRubricTextMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Rubric'
}

{ #category : #private }
GLMRubricTextMorphicTest >> checkInitializeBlockSetsTextSelectionForPresentation: aSelector [
	| browser textMorph |
	
	browser := GLMTabulator new.
	browser act: [ :aBrowser | aBrowser entity: '2 + 3 + 4' ] 
		icon: GLMUIThemeExtraIcons glamorousRefresh 
		entitled: 'Change text'.
	browser row: #main.
	browser transmit to: #main; andShow: [ :c |
		(c perform: aSelector)
			title: 'Code';
			initialize: [ :script |
				script selectionInterval: (1 to: 5) ] ].
	window := browser openOn: ('1 + 2 + 3').

	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: textMorph getSelectionFromModel equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).
	
	browser entity: '2 + 3 + 4'.
	self assert: textMorph getSelectionFromModel equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).
]

{ #category : #tests }
GLMRubricTextMorphicTest >> noTestAcceptKeyCanBeOverriden [
	| composite textMorph overriden shortcut|
	overriden := false.
	composite := GLMCompositePresentation new with: [ :a | a text act: [ :text | overriden := true ] on: $s entitled: 'Override'].
	window := composite openOn: '4'.
	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	(Delay forMilliseconds: HandMorph doubleClickTime + 1) wait.
	"self simulateOnlyOneClickOn: textMorph."
	self assert: ActiveHand keyboardFocus equals: textMorph textArea.
	
	shortcut := $s meta asKeyCombination.
	textMorph textArea handleKeystroke: (KeyboardEvent new
		setType: #keystroke
		buttons: shortcut modifier eventCode
		position: textMorph textArea center
		keyValue: shortcut key asciiValue
		charCode: shortcut key asciiValue
		hand: nil
		stamp: DateAndTime now).
	self assert: overriden
]

{ #category : #tests }
GLMRubricTextMorphicTest >> noTestEnteringTextInPort [
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	self simulateKeyStrokes: 'hello'.
	self assert: (composite pane port: #text) value asString equals: 'hello'
]

{ #category : #tests }
GLMRubricTextMorphicTest >> noTestExplicitAcceptDoesNotAffectTextPort [
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	self simulateKeyStrokes: '4'.
	self assert: (composite pane port: #text) value asString equals: '1234'.
	self simulateKeyStroke: $s meta.
	self simulateKeyStrokes: '56'.
	self assert: (composite pane port: #text) value asString equals: '123456'
]

{ #category : #tests }
GLMRubricTextMorphicTest >> noTestOKeyWithCtrlCannotBeOverriden [
	| composite textMorph overriden shortcut|
	overriden := false.
	composite := GLMCompositePresentation new with: [ :a | a text act: [ :text | overriden := true ] on: $o entitled: 'Override'].
	window := composite openOn: '4'.
	textMorph := self find: RubScrolledTextMorph in: window.
	"self simulateOnlyOneClickOn: textMorph."
	
	shortcut := $o meta asKeyCombination.
	textMorph textArea handleKeystroke: (KeyboardEvent new
		setType: #keystroke
		buttons: shortcut modifier eventCode
		position: textMorph textArea center
		keyValue: shortcut key asciiValue
		charCode: shortcut key asciiValue
		hand: nil
		stamp: DateAndTime now).
	
	self assert: overriden not
]

{ #category : #private }
GLMRubricTextMorphicTest >> simulateOnlyOneClickOn: aMorph [
	"(Delay forMilliseconds: HandMorph doubleClickTime + 1) wait."
	aMorph simulateClick
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testInitializeBlockSetsTextSelectionForMethodPresentation [
	
	self checkInitializeBlockSetsTextSelectionForPresentation: #pharoMethod
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testInitializeBlockSetsTextSelectionForScriptPresentation [

	self checkInitializeBlockSetsTextSelectionForPresentation: #pharoScript
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testInitializeBlockSetsTextSelectionForTextPresentation [
	
	self checkInitializeBlockSetsTextSelectionForPresentation: #text
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testMultipleSelectionWithChange [
	| composite textMorph textPresentation |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self assertEmpty: textMorph selectionInterval.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	self assert: textMorph textArea selection asString equals: '2345'.
	self assert: textPresentation selectedText asString equals: '2345'
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testPastingUpdatesTextPort [
	| composite textMorph  |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 42.
	textMorph := self find: RubEditingArea in: window.
	Clipboard clipboardText: '42'.
	textMorph editor paste.
	self assert: (composite pane port: #text) value asString equals: '42'
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testResettingTheSelection [
	| composite textPresentation |
	composite := GLMCompositePresentation new
		with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	textPresentation selectionInterval: nil
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testTextPortPopulatedByDefault [
	| composite |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testUnregisterFromAnnouncementsWhenUpdating [
	| composite announcingCollection numberOfExecutions |
	numberOfExecutions := 0.
	announcingCollection := GLMAnnouncingCollection new.
	composite :=  GLMCompositePresentation new with: [ :a | 
					(a text)
						updateOn: GLMItemAdded from: [ announcingCollection ];
						display: [ 
								numberOfExecutions := numberOfExecutions + 1. 
								numberOfExecutions asString ] ].
	window := composite openOn: announcingCollection.
	1 to: 3 do: [ :each | announcingCollection add: each ].
	self assert: numberOfExecutions equals: 4	"it should not be 8"
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testUpdatingScriptPresentationPreservesSelection [
	| composite textPresentation textMorph |
	
	composite := GLMCompositePresentation new with: [ :c | 
		c pharoScript
			title: 'Code';
			act: [ :aPresentation | aPresentation update ] 
				icon: GLMUIThemeExtraIcons glamorousRefresh 
				entitled: 'Update presentation'  ].
	
	window := composite openOn: '1 + 2 + 3'.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (1 to: 5).
	textMorph := self find: RubScrolledTextMorph in: window.
	
	self assert: textPresentation selectionInterval equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).
	
	self assert: textMorph getSelectionFromModel equals: (1 to: 0).
	
	textPresentation update.
	
	self assert: textPresentation selectionInterval equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).
	
	self assert: textMorph getSelectionFromModel equals: (1 to: 0).
]

{ #category : #tests }
GLMRubricTextMorphicTest >> testUpdatingTextPortUpdatesTheMorph [
	<expectedFailure>
	| composite textMorph |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: 'fourtytwo'.
	composite first text: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'.
	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: textMorph text asString equals: '42'
]
