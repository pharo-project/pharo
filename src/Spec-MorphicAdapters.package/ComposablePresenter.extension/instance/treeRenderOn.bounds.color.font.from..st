*Spec-MorphicAdapters
treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph
	"Specify how this object as a list item should be drawn"

	^ self ensureMorphicAdapterDo: [ :adapter | adapter 
		treeRenderOn: aCanvas 
		bounds: drawBounds 
		color: drawColor 
		font: aFont 
		from: aMorph ]