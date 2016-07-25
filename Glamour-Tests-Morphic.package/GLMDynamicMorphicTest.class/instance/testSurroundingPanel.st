tests
testSurroundingPanel
	| browser dynamicContainerMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | a dynamic presentation: [:some :object | some text ]].
	window := browser openOn: 4.

	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first isKindOf: RubScrolledTextMorph)