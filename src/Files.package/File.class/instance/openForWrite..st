open/close
openForWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	| fileHandle |
	fileHandle := self basicOpenForWrite: writeMode.
	fileHandle ifNil: [ FileException signal: 'cannot open file: ', name ].
	^ (BinaryFileStream handle: fileHandle file: self forWrite: writeMode)
		register;
		yourself