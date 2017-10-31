tests
testOutputRemoval

	| output transcript |
	
	transcript := TranscriptStream new.
	
	output := TestableTranscriptOutput new.
	
	transcript addOutput: output.
	
	self 
	assert: (transcript outputs includes: output);
	assert: (output transcripts includes: transcript).
	
	transcript outputs remove: output.
	
	self assert: (transcript outputs asArray isEmpty)
	