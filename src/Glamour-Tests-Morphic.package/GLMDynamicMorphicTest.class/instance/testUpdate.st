tests
testUpdate
	| browser dynamicContainerMorph presentationObject |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | 
		a dynamic presentation: [:some :object | 
			some custom: presentationObject ]].
	presentationObject := GLMRubricTextPresentation new.
	window := browser openOn: 42.
	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first isKindOf: RubScrolledTextMorph).
	presentationObject := GLMListPresentation new.
	browser update.
	dynamicContainerMorph := window submorphs last submorphs first submorphs first.
	self assert: (dynamicContainerMorph isKindOf: PanelMorph).
	self assert: (dynamicContainerMorph submorphs first submorphs first isKindOf: MorphTreeMorph)