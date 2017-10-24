tests - converting
testRepeat

	self 
	assert: ('123' repeat: 1) = '123';
	assert: ('123' repeat: 2) = '123123';
	assert: ('123' repeat: 0) = '';
	should: [('123' repeat: -1)] raise: Error.