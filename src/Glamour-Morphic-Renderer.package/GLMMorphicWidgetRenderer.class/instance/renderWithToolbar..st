private
renderWithToolbar: aPresentation
	| presentationMorph |
	presentationMorph := self renderObject: aPresentation.
	^ self morph: presentationMorph withToolbarOf: aPresentation