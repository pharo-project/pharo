tests
testInitializations

	self assert: Transcript class == TranscriptStream .

	self assert: (
		SessionManager default hasRegistered: TranscriptStream name 
	)
	