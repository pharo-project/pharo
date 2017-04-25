scripting
column: aBlockOrString evaluated: aBlock width: aNumber tags: anObject
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		width: aNumber;
		tags: anObject;
		yourself)	