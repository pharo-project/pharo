adding
addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar action: anActionMorphOrNil
	| newPage label |
	newPage := LazyTabPage new.
	newPage
		lazyPageMorphCreation: aBlock; 
		labelMorph: aStringOrMorph; 
		toolbar: aToolbar.
	label := self 
			buildLabelFrom: aStringOrMorph 
			withAction: anActionMorphOrNil.
	self 
		addPage: newPage
		label: label