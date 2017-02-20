rendering
rowMorphForColumn: aColumn item: anItem withIndex: aRowIndex
	| rowElements |
	rowElements := OrderedCollection new.
	
	(self table columns first == aColumn) ifTrue: [
		self rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements ].
	
	rowElements
		addAll: (self 
			rowMorphElementsForColumn: aColumn glamourColumn 
			element: (self dataFromPresentationItem: anItem)
			withIndex: aRowIndex ).
		
	^ Smalltalk ui theme newRowIn: World for: rowElements