private
renderWithTitle: aPresentation
	| morph |
	morph := self renderObject: aPresentation.
	^ self morph: morph withTitleOf: aPresentation