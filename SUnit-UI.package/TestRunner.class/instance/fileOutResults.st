actions
fileOutResults
	"Save current test results into text file."
	"Generate a filename like 'tests_Version_23_May_2010_9-19-20_am.txt' "

	| fileName defaultFilename stream printer |
	defaultFilename := self generateDefaultFileOutResultsName.
						
						
	fileName := UIManager default 
		request: 'Enter the file name' 
		initialAnswer: defaultFilename.
	fileName ifNil: [^ self].
	stream := MultiByteFileStream newFileNamed: fileName.	
	[result fileOutOn: stream] 
		ensure: [stream close].