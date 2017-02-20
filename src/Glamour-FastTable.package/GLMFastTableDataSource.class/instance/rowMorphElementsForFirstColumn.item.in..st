accessing
rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements
	| icon |
	
	icon := self glamourPresentation icon cull: anItem.
	icon ifNotNil: [ rowElements add: icon asMorph ] 