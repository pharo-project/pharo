private
morph: aMorph withTitleOrNilOf: aPresentation
	(aPresentation hasTitle or: [aPresentation hasTitleIcon]) ifFalse: [
		"we render at least the toolbar"
		^ self morph: aMorph withToolbarOf: aPresentation ].
	^ self morph: aMorph withTitleOf: aPresentation