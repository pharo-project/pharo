scripting
column: aBlockOrString evaluated: aBlock width: aNumber tags: anObject sortedBy: aSortingBlock
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		tags: anObject;
		sortBlock: aSortingBlock
		yourself)	