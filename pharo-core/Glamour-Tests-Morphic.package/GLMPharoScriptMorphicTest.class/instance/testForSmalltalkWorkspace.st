tests
testForSmalltalkWorkspace
	| browser textMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a pharoScript variableBindings: [:x | {#theNumber -> x}]].
	window := browser openOn: 42.

	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: (textMorph model hasBindingOf: #theNumber)