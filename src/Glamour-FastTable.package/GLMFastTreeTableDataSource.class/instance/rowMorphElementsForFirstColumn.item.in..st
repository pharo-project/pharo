accessing
rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements

	anItem depth timesRepeat: [ rowElements add: self class emptyMorph ].
	rowElements add: (self buttonFor: anItem).
	(self iconFor: anItem data) ifNotNil: [ :icon | rowElements add: icon asMorph ].