layout-properties
vResizing: aSymbol
	"Layout specific. This property describes how the receiver should be resized with respect to its owner and its children. Possible values are:
		#rigid			-	do not resize the receiver
		#spaceFill		-	resize to fill owner's available space
		#shrinkWrap	- resize to fit children
	"
	self assureLayoutProperties vResizing: aSymbol.
	self layoutChanged.
