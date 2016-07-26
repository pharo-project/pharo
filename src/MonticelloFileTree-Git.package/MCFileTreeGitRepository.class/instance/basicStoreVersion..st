storing
basicStoreVersion: aVersion
	"Do a filetree store followed by in order, a check and save dependencies, a commit and an update of the version info."

	"Recover the last commit id and update the version info to have a correctly synchronized in image id (apart from the version number?)"

	| packageDirectoryString output deps gitCommand |
	self internalStoreVersion: aVersion.
	deps := self internalStoreDependencies: aVersion.
	packageDirectoryString := Array
		streamContents:
			[ :stream | 
			{aVersion} , deps
				do:
					[ :each | 
					stream
						nextPut: (self class parseName: each info name) first , self packageExtension ] ].
	gitCommand := Array
		streamContents:
			[ :stream | 
			stream nextPutAll: #('commit' '--allow-empty-message').
			(self class namesAt: aVersion info author)
				ifNotNil:
					[ :n | stream nextPut: '--author=' , (n at: #name) , ' ' , (n at: #email) ].
			stream
				nextPut: '-m';
				nextPut: (self escapeForShell: aVersion info message convertToSystemString);
				nextPut: '--';
				nextPutAll: packageDirectoryString ].
	self gitCommand: gitCommand in: directory.
	{aVersion} , deps
		do:
			[ :each | 
			| b |
			(b := MCWorkingCopy hasPackageNamed: each package name) == false
				ifFalse:
					[ b
						versionInfo: (self versionInfoFromVersionNamed: each package name) ] ]