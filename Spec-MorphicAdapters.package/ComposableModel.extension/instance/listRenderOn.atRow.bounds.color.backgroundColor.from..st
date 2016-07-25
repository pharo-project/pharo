*Spec-MorphicAdapters
listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph
	"Specify how this object as a list item should be drawn"

	^ self ensureMorphicAdapterDo: [ :adapter | adapter
		listRenderOn: aCanvas
		atRow: aRow
		bounds: drawBounds
		color: drawColor
		backgroundColor: backgroundColor
		from: aMorph ]