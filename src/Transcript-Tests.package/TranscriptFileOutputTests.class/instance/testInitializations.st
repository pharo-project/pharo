tests
testInitializations

	self assert: (
		SessionManager default hasRegistered: TranscriptFileOutput name 
	)