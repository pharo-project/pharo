tests
testAllFileTyped
	self assert: (self commandLine allFilesWithExtension: #txt) = #('noOpt2.txt' 'opt12.txt').
	self assert: (self commandLine allFilesWithExtension: #foo) isEmpty.