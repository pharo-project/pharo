printing
debugPrint

	^(WriteStream on: (String new: 10))
		print: self class;
		nextPutAll: ' (';
		print: self date;
		nextPutAll: ' - ';
		print: self bounds;
		nextPut: $);
		contents