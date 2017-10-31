tests
testSpecialFile

	| output transcript |
	
	output := TranscriptSpecialFileOutput forFileNamed: nil.
	
	transcript := TranscriptStream new.
	
	transcript 
	addOutput: output;
	addOutput: output.
	
	self 
	assert: transcript outputs = transcript fileOutputs;
	assert: transcript outputs size = 1;
	assert: transcript outputs anyOne = output.
	
	transcript removeOutput: output.
	
	self 
	assert: transcript outputs size = 0;
	assert: transcript outputs = transcript fileOutputs.
	
