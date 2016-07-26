scripting
column: aBlockOrString evaluated: aBlock
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock)