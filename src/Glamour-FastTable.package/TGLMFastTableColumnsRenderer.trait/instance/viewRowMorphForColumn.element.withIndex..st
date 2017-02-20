rendering
viewRowMorphForColumn: aGlamourColumn element: anElement withIndex: anInteger
	^ (self 
		valueFor: (self glamourPresentation isWithElementIndex
			ifTrue: [ 
				anElement asGlamorousMultiValue,
				anInteger asGlamorousMultiValue ]
			ifFalse: [ anElement ])
		in: aGlamourColumn) asMorph asReadOnlyMorph