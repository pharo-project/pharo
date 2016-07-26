factory
viewRowMorphForColumn: glamourColumn element: element
	^ (self glamourPresentation column: glamourColumn valueFor: element) asMorph asReadOnlyMorph