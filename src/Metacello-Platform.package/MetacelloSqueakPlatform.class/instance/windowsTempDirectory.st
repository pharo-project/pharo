file system
windowsTempDirectory
	Smalltalk at: #OSProcess ifPresent: [:osProcess |
		^ osProcess thisOSProcess environment at: 'TMP'].
	(FileDirectory on: 'C:\Windows') exists
		ifTrue: ["C:\Windows\Temp is writable, but not listable and #exists answers false for it"
			"create a subdirectory in it (for which #exists will answer true) and use that"
			| metacelloDir | metacelloDir := FileDirectory on: 'C:\Windows\Temp\Metacello'.
			metacelloDir exists ifFalse:
				[metacelloDir containingDirectory createDirectory: metacelloDir localName].
			^ metacelloDir pathName]
		ifFalse: [self error: 'Cannot determine temporary directory. Please install OSProcess.']