testing
testAbsolutePath

	#('c:\' 'C:\temp' 'A:\temp\test' '\test\bar' '\') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isAbsolute ] 
	