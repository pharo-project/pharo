factory
rowMorphForColumn: glamourColumn element: element
	| rowElements contentMorph tagMorphs |
		
	contentMorph := glamourColumn modifiedBlock
		ifNotNil: [ self editRowMorphForColumn: glamourColumn element: element ]
		ifNil: [ self viewRowMorphForColumn: glamourColumn element: element ].
	
	tagMorphs := self tagMorphsFrom: glamourColumn for: element.
	tagMorphs ifEmpty: [ ^ contentMorph ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		addAll: tagMorphs;
		yourself.
	
	^ (UITheme current newRowIn: World for: rowElements)	