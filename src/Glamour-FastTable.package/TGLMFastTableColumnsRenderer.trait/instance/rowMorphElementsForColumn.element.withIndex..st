rendering
rowMorphElementsForColumn: glamourColumn element: element withIndex: aRowIndex 
	| rowElements contentMorph tagMorphs |
		
	contentMorph := glamourColumn modifiedBlock
		ifNotNil: [ self editRowMorphForColumn: glamourColumn element: element withIndex: aRowIndex ]
		ifNil: [ self viewRowMorphForColumn: glamourColumn element: element withIndex: aRowIndex ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		yourself.
	
	tagMorphs := self tagMorphsFrom: glamourColumn for: element.
	tagMorphs ifNotEmpty: [
		rowElements addAll: tagMorphs ].
	
	^ rowElements