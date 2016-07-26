initialize-release
initialize
	super initialize.
	family := 'monospace'.
	options := Dictionary new.
	options
		at:#weight put: #normal; 
		at:#slant put: #normal.
	size := 14.
	