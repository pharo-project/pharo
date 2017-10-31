tests
testTestableOutput

	| tts |
	
	tts := TestableTranscriptOutput new.
	
	self 
	assert: tts apiCount = self defaultSubscriberApiCount;
	assert: tts data = '';
	assert: tts transcripts isEmpty.
	
	tts transcriptEnded: nil withEntry: 'ThisIsAMockString'.
	
	self 
	assert: tts apiCount = { 0 . 1 . 0 . 0 . 0 };
	assert: tts data = 'ThisIsAMockString'.
	
	tts 
	transcriptCleared: nil;
	transcriptClosed: nil;
	transcriptUpdate: nil.
	
	self 
	assert: tts apiCount = { 0 . 1 . 1 . 1 . 1 };
	assert: tts data = 'ThisIsAMockString'.
	
	tts transcriptAddedAsOutputIn: 43.
	
	self 
	assert: tts apiCount = { 1 . 1 . 1 . 1 . 1 };
	assert: tts data = 'ThisIsAMockString';
	assert: tts transcripts size = 1;
	assert: tts transcripts anyOne = 43.
	
