initialization
setHash: aHash 
	hash := aHash.
	ipad := ByteArray 
		new: aHash blockSize
		withAll: 54.
	epad := ByteArray 
		new: aHash blockSize
		withAll: 92