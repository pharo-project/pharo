initialization
initialize

	| aFile |

	super initialize.
	suiteTime := 0 asDuration.
	suitePosition := suiteFailures := suiteErrors := 0.
	
	aFile := File named: 'progress.log'.
	aFile delete.
	progressFile := ZnCrPortableWriteStream on: (
		ZnCharacterWriteStream
			on: (aFile writeStream setToEnd; yourself)
			encoding: 'utf8').	