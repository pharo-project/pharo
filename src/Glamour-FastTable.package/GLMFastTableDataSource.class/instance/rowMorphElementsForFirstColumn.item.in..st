accessing
rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements
	
	(self iconFor: anItem) ifNotNil: [ :anIcon | rowElements add: anIcon asMorph ] 