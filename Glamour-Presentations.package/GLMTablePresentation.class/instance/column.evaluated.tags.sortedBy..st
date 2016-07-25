scripting
column: aBlockOrString evaluated: aBlock tags: anObject sortedBy: aSortingBlock
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tags: anObject;
		sortBlock: aSortingBlock)