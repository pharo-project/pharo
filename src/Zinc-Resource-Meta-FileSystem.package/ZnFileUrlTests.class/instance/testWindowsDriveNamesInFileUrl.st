testing
testWindowsDriveNamesInFileUrl
	"Note the 3 slashes: 2 are from the scheme, file://, 1 is from the empty host and starts the absolute path"
	
	SystemVersion current major < 3 ifTrue: [ self skip ].
	self 
		assert: 'file:///C:/Programs/notepad.exe' asUrl asFileReference
		equals: FileLocator C / 'Programs' / 'notepad.exe'.
	self 
		assert: 'file:///C:' asUrl asFileReference
		equals: FileLocator C