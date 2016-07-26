host window access
hostWindowTitle: aString 
	self 
		primitiveWindowTitle: self hostWindowIndex 
		string: (UTF8TextConverter default convertFromSystemString: aString)