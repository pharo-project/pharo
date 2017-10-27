actions
fileOutResults
	"Save current test results into text file."
	"Generate a filename like 'tests_Version_23_May_2010_9-19-20_am.txt' "

	| fileName defaultFilename stream |
	defaultFilename := self generateDefaultFileOutResultsName.
						
						
	fileName := UIManager default 
		request: 'Enter the file name' 
		initialAnswer: defaultFilename.
	fileName ifNil: [^ self].
	stream := ZnCrPortableWriteStream on: (ZnCharacterWriteStream
			on: (File openForWriteFileNamed: fileName)
			encoding: 'utf8').
	[result fileOutOn: stream]
		ensure: [stream close].