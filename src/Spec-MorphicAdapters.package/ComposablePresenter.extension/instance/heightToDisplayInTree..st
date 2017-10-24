*Spec-MorphicAdapters
heightToDisplayInTree: aTree
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter heightToDisplayInTree: aTree ]