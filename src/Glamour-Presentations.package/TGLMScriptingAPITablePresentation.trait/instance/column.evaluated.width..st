scripting
column: aBlockOrString evaluated: aBlock width: aNumber 
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		yourself)	