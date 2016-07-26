tests
testDefault
	| browser textMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a pharoScript display: '123456789'].
	window := browser openOn: 4.

	textMorph := window submorphs last submorphs first submorphs first.
	self assert: (textMorph isKindOf: RubScrolledTextMorph)