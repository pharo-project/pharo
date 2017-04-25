rendering
viewRowMorphForColumn: aGlamourColumn element: anElement withIndex: anInteger
	^ (self valueFor: anElement withIndex: anInteger in: aGlamourColumn) asMorph asReadOnlyMorph