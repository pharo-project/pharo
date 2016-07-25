ui
gtDisplayOn: aStream
	self withExampleDo: [ self example gtDisplayOn: aStream ] ifAbsent: [ 
		aStream nextPutAll: 'anExample ('.
		aStream nextPutAll: self provider class name; nextPutAll: '>>#'; nextPutAll: self selector.
		aStream nextPutAll: ')' ]