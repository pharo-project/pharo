actions
hideDetail
	detailMorph ifNil: [ ^ false ].
	self removeMorph: detailMorph.
	detailMorph delete.
	detailMorph := nil.
	self changed.
	^ true