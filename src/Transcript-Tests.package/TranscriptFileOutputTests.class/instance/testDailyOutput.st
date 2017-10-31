tests
testDailyOutput

	| dailyOutput |

	self assert: self dailyFileReference exists not.
	
	dailyOutput := TranscriptZDailyFileOutput forFileNamed: self fileName.
	
	dailyOutput transcriptEnded: nil withEntry: ':-)'.
		
	self 
	assert: self dailyFileReference exists;
	assert: dailyOutput datedFileName = self dailyFileName.

	dailyOutput transcriptClosed: nil.

	self dailyFileReference delete.	
	self assert: self dailyFileReference exists not.