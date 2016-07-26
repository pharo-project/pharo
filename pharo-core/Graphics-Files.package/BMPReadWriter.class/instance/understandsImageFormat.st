testing
understandsImageFormat
	stream size < 54 ifTrue:[^false]. "min size = BITMAPFILEHEADER+BITMAPINFOHEADER"
	self readHeader.
	bfType = 19778 "BM" ifFalse:[^false].
	biSize = 40 ifFalse:[^false].
	biPlanes = 1 ifFalse:[^false].
	bfSize <= stream size ifFalse:[^false].
	biCompression = 0 ifFalse:[^false].
	^true