drawing
drawSeparatorAfter: aRow on: aCanvas

	| frame rectangle height separatorColor |
	height := listSource separatorSize.
	separatorColor := listSource separatorColor.
	frame := self selectionFrameForRow: aRow.
	rectangle := (frame left@(frame bottom - height)) corner: (frame right@frame bottom).
	aCanvas fillRectangle: rectangle color:  separatorColor