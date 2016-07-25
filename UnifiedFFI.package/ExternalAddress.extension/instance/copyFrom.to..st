*UnifiedFFI
copyFrom: startIndex to: endIndex
	| result size |
	size := endIndex - startIndex + 1.
	result := ByteArray new: size. 
	LibC memCopy: (self + (startIndex - 1)) to: result size: size.
	^ result