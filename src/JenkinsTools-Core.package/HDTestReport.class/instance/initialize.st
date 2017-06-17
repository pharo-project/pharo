initialization
initialize
	super initialize.
	suiteTime := 0 asDuration.
	suitePosition := suiteFailures := suiteErrors := 0.
	progressFile := StandardFileStream forceNewFileNamed: 'progress.log'
	