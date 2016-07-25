initialization
vertices: verts color: aColor borderWidth: borderWidthInteger borderColor: anotherColor 
	super initialize.
	vertices := verts.
	color := aColor.
	borderWidth := borderWidthInteger.
	borderColor := anotherColor.
	closed := vertices size > 2.
	arrows := #none.
	self computeBounds