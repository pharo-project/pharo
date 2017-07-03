private
writeError: error stack: stack
	suiteErrors := suiteErrors + 1.
	stream tab; tab; nextPutAll: '<error type="'.
	self writeException: error stack: stack.
	stream nextPutAll: '</error>'; lf