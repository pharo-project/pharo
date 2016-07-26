testing
testCommentInString
	self 
		assert: (STONCStyleCommentsSkipStream on: '''/*comment*/''' readStream) upToEnd 
		equals: '''/*comment*/'''.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{''foo'':''/*comment*/bar''}' readStream) upToEnd 
		equals: '{''foo'':''/*comment*/bar''}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{''foo'':''//comment\''bar''}' readStream) upToEnd 
		equals: '{''foo'':''//comment\''bar''}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{"foo":"/*comment*/bar"}' readStream) upToEnd 
		equals: '{"foo":"/*comment*/bar"}'.
	self 
		assert: (STONCStyleCommentsSkipStream on: '{"foo":"//comment\"bar"}' readStream) upToEnd 
		equals: '{"foo":"//comment\"bar"}'