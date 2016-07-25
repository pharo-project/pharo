dispatch
listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph
	"Specify how this object as a list item should be drawn"

	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	self widget
		listRenderOn: aCanvas
		atRow: aRow
		bounds: drawBounds
		color: drawColor
		backgroundColor: backgroundColor
		from: aMorph