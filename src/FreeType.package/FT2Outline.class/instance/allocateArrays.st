private
allocateArrays
	" allocate the arrays for the primLoadArraysFrom:"
	points := 	IntegerArray new: pointsSize * 2.
	tags := 		ByteArray new: pointsSize.
	contours := 	ShortIntegerArray new: contoursSize.