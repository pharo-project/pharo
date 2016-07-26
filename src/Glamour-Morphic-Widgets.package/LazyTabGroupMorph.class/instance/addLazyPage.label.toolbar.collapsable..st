adding
addLazyPage: aBlock label: aStringOrMorph toolbar: aToolbar collapsable: aBoolean
	| newPage label |
	newPage := LazyTabPage new.
	newPage
		lazyPageMorphCreation: aBlock; 
		labelMorph: aStringOrMorph; 
		toolbar: aToolbar.
	label := self 
			buildLabelFrom: aStringOrMorph 
			withAction: (aBoolean 
				ifTrue: [GLMActionButtonBrick new
					action: [self removePage: newPage];
					icon: self theme windowCloseForm ]
				ifFalse: [nil]).
	self 
		addPage: newPage
		label: label.
	