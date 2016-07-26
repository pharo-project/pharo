testing
testRelativePath

	#('a' 'bin\foo' 'temp\test' 'C:temp\test') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isRelative ] 
	