scripting
column: aBlockOrString evaluated: aBlock modified: aModifiedBlock
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString;
		computation: aBlock;
		modifiedBlock: aModifiedBlock)