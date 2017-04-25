scripting
column: aBlockOrString evaluated: aBlock sortedBy: aSortingBlock
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		sortBlock: aSortingBlock)