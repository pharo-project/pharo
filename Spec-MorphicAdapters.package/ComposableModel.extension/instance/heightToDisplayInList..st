*Spec-MorphicAdapters
heightToDisplayInList: aList
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter heightToDisplayInList: aList ]