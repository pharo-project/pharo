initialization
initialize
	
	super initialize.
	kernelClasses := self kernelClassesToExclude asOrderedCollection.
	kernelClasses := kernelClasses 
		addAll: (kernelClasses collect: #class);
		yourself
	