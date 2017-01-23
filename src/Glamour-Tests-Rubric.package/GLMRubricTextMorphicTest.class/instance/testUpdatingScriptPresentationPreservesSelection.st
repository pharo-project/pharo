tests
testUpdatingScriptPresentationPreservesSelection
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