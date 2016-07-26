*CodeExport
printMethodChunk: selector on: outStream
	"Copy the source code for the method associated with selector onto the fileStream."

	| preamble method |
	preamble := self name, ' methodsFor: ', (self organization categoryOfElement: selector) asString printString.
	method := self methodDict
		at: selector
		ifAbsent: [ 
			outStream
				nextPutAll: selector;
				cr.
			outStream
				tab;
				nextPutAll: '** ERROR!  THIS SCRIPT IS MISSING ** ' translated;
				cr;
				cr.
			outStream nextPutAll: '  '.
			^ outStream ].

	"Print the preamble"
	outStream cr; nextChunkPut: ''.
	outStream nextChunkPut: (String streamContents: [ :strm | 
		strm nextPutAll: preamble.
		method stamp ifNotEmpty: [ :stamp | 
			strm
				nextPutAll: ' stamp: ';
				print: stamp ] ]).
	outStream cr.

	"Print the source code"
	outStream 
		nextChunkPut: method sourceCode;
		nextChunkPut: ' '; cr.
	
	^ outStream