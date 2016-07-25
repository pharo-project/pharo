initialization
initializeContainer: aTableContainerMorph left: aColumn right: otherColumn
	self initialize.
	self width: self class resizerWidth.
	container := aTableContainerMorph.
	leftColumn := aColumn.
	rightColumn := otherColumn
	