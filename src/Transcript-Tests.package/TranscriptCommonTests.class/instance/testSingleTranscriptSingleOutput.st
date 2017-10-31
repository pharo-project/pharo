tests
testSingleTranscriptSingleOutput

	| transcript output |
	
	transcript := TranscriptStream new.
	output := TestableTranscriptOutput new.
	
	transcript addOutput: output.
	
	self 
	assert: output transcripts size = 1;
	assert: output transcripts first = transcript;
	assert: output apiCount = { 1 . 0 . 0 . 0 . 0}.
	
	transcript 
	show: 'ThisIsSomeCharacters'; 
	nextPutAll: 'abcdefg';
	clear;
	nextPutAll: 'hijkl';
	flush.
	
	self 
	assert: output apiCount = { 1 . 2  . 1 . 0 . 0 };
	assert: output data = 'ThisIsSomeCharactershijkl'.