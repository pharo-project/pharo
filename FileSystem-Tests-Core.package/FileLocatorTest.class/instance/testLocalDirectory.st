resolution tests
testLocalDirectory
	locator := FileLocator localDirectory.
	self assert: locator resolve fullName equals: (FileLocator imageDirectory resolve / (SystemResolver defaultLocalDirectoryName)) fullName