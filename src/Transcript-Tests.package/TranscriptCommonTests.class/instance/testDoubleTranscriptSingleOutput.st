tests
testDoubleTranscriptSingleOutput

	| transcriptA transcriptB output |
	
	transcriptA := TranscriptStream new.
	transcriptB := TranscriptStream new.
	output := TestableTranscriptOutput new.
	
	transcriptA addOutput: output.	
	transcriptB addOutput: output.
	
	self 
	assert: output apiCount = { 2 . 0  . 0 . 0 . 0 };
	assert: output transcripts size = 2;
	assert: (output transcripts includesAllOf: { transcriptA . transcriptB });
	assert: output data = ''.
	
	
	transcriptA 
	show: 'DataFromA'.
		
	self 
	assert: output apiCount = { 2 . 1  . 0 . 0 . 0 };
	assert: output data = 'DataFromA'.
	
	transcriptB
	show: 'DataFromB'.
	
	self 
	assert: output apiCount = { 2 . 2  . 0 . 0 . 0 };
	assert: output data = 'DataFromADataFromB'.
	
	transcriptB clear.
	
	self assert: output apiCount = { 2 . 2 . 1 . 0 . 0 }.
	
	transcriptB close.
	
	self assert: output apiCount = { 2 . 3 . 1 . 1 . 0 }.
	
	transcriptA clear; close.
	
	self assert: output apiCount = { 2 . 4 . 2 . 2 . 0 }.
