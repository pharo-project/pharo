instance creation
container: aTableContainerMorph left: aColumn right: otherColumn
	^ self basicNew 
		initializeContainer: aTableContainerMorph 
			left: aColumn 
			right: otherColumn;
		yourself