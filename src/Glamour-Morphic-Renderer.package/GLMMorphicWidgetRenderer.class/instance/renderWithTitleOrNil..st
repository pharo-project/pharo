private
renderWithTitleOrNil: aPresentation
	| morph |
	morph := self renderObject: aPresentation.
	^ self morph: morph withTitleOrNilOf: aPresentation