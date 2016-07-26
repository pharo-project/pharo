testing
testSTON
	"Comments are processed first and totally removed.
	They are not even whitespace (not that whitespace is relevant in STON)."
	
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '123/*comment*/456' readStream)) 
		equals: 123456.
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '/*comment*/''abcd''' readStream)) 
		equals: 'abcd'.
	self 
		assert: (STON fromStream: (STONCStyleCommentsSkipStream on: '123//456' readStream)) 
		equals: 123.
	self 
		should: [ STON fromStream: (STONCStyleCommentsSkipStream on: '//456' readStream) ] 
		raise: STONReaderError
