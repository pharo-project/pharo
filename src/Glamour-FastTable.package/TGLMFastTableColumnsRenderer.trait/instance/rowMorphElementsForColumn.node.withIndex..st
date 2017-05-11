rendering
rowMorphElementsForColumn: glamourColumn node: aNode withIndex: aRowIndex 
	| rowElements contentMorph tagMorphs |
		
	contentMorph := glamourColumn modifiedBlock
		ifNotNil: [ self editRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex ]
		ifNil: [ self viewRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		yourself.
	
	tagMorphs := self tagMorphsFrom: glamourColumn for: aNode.
	tagMorphs ifNotEmpty: [
		rowElements addAll: tagMorphs ].
	
	^ rowElements