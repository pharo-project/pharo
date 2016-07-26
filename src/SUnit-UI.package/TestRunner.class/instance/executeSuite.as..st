processing
executeSuite: aTestSuite as: aBlock
" The block defines how to interact with the suite. run with a result or debug "
	aTestSuite subscribe:TestAnnouncement do: self updateUIBlock.
	[ aBlock cull: aTestSuite cull: result  ] ensure: [
		aTestSuite unsubscribe:TestAnnouncement.
	].


