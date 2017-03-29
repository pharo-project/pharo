rendering
viewRowMorphForColumn: aGlamourColumn node: aNode withIndex: anInteger
	^ (self valueFor: aNode withIndex: anInteger in: aGlamourColumn) asMorph asReadOnlyMorph