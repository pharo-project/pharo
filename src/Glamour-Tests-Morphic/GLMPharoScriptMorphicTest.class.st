Class {
	#name : #GLMPharoScriptMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMPharoScriptMorphicTest >> testDefault [
	| browser textMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a pharoScript display: '123456789'].
	window := browser openOn: 4.

	textMorph := window submorphs last submorphs first submorphs first.
	self assert: (textMorph isKindOf: RubScrolledTextMorph)
]

{ #category : #tests }
GLMPharoScriptMorphicTest >> testExplicitAcceptDoesNotAffectTextPort [
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a pharoScript display: '123' ].
	window := composite openOn: 4.

	window position: 0 @ 0.
	window extent > Display extent ifTrue: [ "ignore test if the display size is too small and we can not simulate events properly" ^ self ].

	textMorph := self find: RubScrolledTextMorph in: window.
	textMorph simulateClick.
	self simulateKeyStrokes: '4'.
	self assert: (composite pane port: #text) value equals: '1234'.
	self simulateKeyStroke: $s meta.
	self simulateKeyStrokes: '56'.
	self assert: (composite pane port: #text) value equals: '123456'
]

{ #category : #tests }
GLMPharoScriptMorphicTest >> testForSmalltalkWorkspace [
	| browser textMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a pharoScript variableBindings: [:x | {#theNumber -> x}]].
	window := browser openOn: 42.

	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: (textMorph model hasBindingOf: #theNumber)
]
