*Spec-MorphicAdapters
widthToDisplayInList: aList
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter widthToDisplayInList: aList ]