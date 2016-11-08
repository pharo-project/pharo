printing
shortPrintString
	"Return a short (but less precise) print string for use where space is tight."

	| s |
	s := String new writeStream.
	s
		nextPutAll: '(';
		nextPutAll: self class name;
		nextPutAll: ' r: ';
		nextPutAll: (self red roundTo: 0.01) printString;
		nextPutAll: ' g: ';
		nextPutAll: (self green roundTo: 0.01) printString;
		nextPutAll: ' b: ';
		nextPutAll: (self blue roundTo: 0.01) printString;
		nextPutAll: ')'.
	^ s contents