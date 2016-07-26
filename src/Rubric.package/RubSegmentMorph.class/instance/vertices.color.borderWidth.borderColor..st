initialization
vertices: verts color: aColor borderWidth: borderWidthInteger borderColor: anotherColor 
	super initialize.
	vertices := verts.
	color := aColor.
	borderWidth := borderWidthInteger. 
	borderColor := anotherColor.
	self computeBounds