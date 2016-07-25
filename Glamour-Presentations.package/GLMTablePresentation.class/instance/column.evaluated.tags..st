scripting
column: aBlockOrString evaluated: aBlock tags: anObject 
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tags: anObject)