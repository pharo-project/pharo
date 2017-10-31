tests
testAddedAsOutput

	| output transcript |
	
	output := TestableTranscriptRubricOutput new.
	transcript := TranscriptStream new.
	
	2 timesRepeat: [
		
		transcript addOutput: output.
	
		self 
		assert: output connections asArray size = 1;
		assert: (output connections includes: transcript);
		assert: transcript outputs size = 1;
		assert: (transcript outputs includes: output).
	
	].

	output removeConnection: transcript.

	self 
	assert: output connections asArray isEmpty;
	assert: transcript outputs isEmpty;
	assert: output noModelStuff.