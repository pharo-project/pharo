printing
printOn: aStream

	"super printOn: aStream. "
	self isValid ifFalse: [ aStream nextPutAll: '**invalid position**'. ^ self ].
	aStream 
		nextPutAll: ' line: ';
		print: self lineNumber;
		nextPutAll: ' col: ';
		print: self linePosition 
		