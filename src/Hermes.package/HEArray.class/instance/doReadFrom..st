reading
doReadFrom: aReader
	| size | 
	size := aReader readInt32.
	value := Array new:size.
	
	1 to:size do:[:idx | value at:idx put: (HEExportedLiteral readFrom: aReader)].