scripting
column: aBlockOrString evaluated: aBlock tags: anObject filtered: anotherObject
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString; 
		computation: aBlock;
		tagsBlock: anObject;
		tagsFilter: anotherObject)