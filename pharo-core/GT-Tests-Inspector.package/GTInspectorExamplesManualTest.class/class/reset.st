public
reset
	FileSystem workingDirectory allFiles 
		select: [ :each | each basename beginsWith: 'gtExamples' ]
		thenDo: [ :each | each delete ]