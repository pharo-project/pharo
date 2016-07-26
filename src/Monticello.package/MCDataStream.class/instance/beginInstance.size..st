write and read
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextNumber: 4 put: anInteger + 1.

	self nextPut: aClass name