private
writeException: error stack: stack.

	stream 
		nextPutAll: (self encode: error class name); 
		nextPutAll: '" message="'; nextPutAll: (self encode: (error messageText ifNil: [ error description ])); 
		nextPutAll: '">'; 
		nextPutAll: (self encode: stack).