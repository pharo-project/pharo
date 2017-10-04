file system
tempFileFor: aName suffix: aSuffixString
	^ (Smalltalk os platformName = 'Win32'
		ifTrue: [self windowsTempDirectory]
		ifFalse: ['/tmp']) , FileDirectory slash , aName , aSuffixString