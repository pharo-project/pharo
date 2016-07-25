private
rowMorphForItem: anItem
	| rowElements |
	rowElements := OrderedCollection new.
	anItem depth timesRepeat: [ rowElements add: self class emptyMorph ].
	rowElements add: (self buttonFor: anItem).
	(self glamourPresentation icon cull: anItem data) ifNotNil: [ :icon | rowElements add: icon asMorph ].
	rowElements
		add: (self glamourPresentation formatedDisplayValueOf: anItem data) asMorph;
		addAll: (self tagMorphsFrom: self glamourPresentation for: anItem data).
	^ Smalltalk ui theme newRowIn: World for: rowElements