tests
testWindowsAbsolutePathName
  
	self assert: (Path from: 'A:\') isAbsolute.
	self assert: (Path from: 'c:\') isAbsolute.
	self assert: (Path from: 'c:\test') isAbsolute.