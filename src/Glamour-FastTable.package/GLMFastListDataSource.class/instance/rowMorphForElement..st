factory
rowMorphForElement: element
	| rowElements |
	rowElements := OrderedCollection new.
	(self glamourPresentation icon cull: element) ifNotNil: [ :icon | rowElements add: icon asMorph ].
	rowElements
		add: (self glamourPresentation formatedDisplayValueOf: element) asMorph asReadOnlyMorph;
		addAll: (self tagMorphsFrom: self glamourPresentation for: element).
	^ Smalltalk ui theme newRowIn: World for: rowElements