testing
testMultiLineComment
	self 
		assert: (STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) upToEnd 
		equals: 'foo\bar' withCRs.
	self 
		assert: (STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) nextLine
		equals: 'foo'.
	self 
		assert: ((STONCStyleCommentsSkipStream on: 'foo\//comment\bar' withCRs readStream) nextLine; nextLine) 
		equals: 'bar'.
	self assert: (STONCStyleCommentsSkipStream on: '//comment' readStream) atEnd.