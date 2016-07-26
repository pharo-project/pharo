printing
printOn: aStream

	"super printOn: aStream. "
	self isValid ifFalse: [ aStream nextPutAll: '**invalid cursor position**'. ^ self ].
	aStream 
		nextPutAll: 'cursor line: ';
		print: self lineNumber;
		nextPutAll: ' col: ';
		print: self linePosition 
		