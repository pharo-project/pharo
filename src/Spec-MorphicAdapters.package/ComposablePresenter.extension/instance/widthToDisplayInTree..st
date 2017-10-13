*Spec-MorphicAdapters
widthToDisplayInTree: aTree
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter widthToDisplayInTree: aTree ]