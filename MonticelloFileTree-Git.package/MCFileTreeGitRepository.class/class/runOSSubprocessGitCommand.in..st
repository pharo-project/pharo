private-commands
runOSSubprocessGitCommand: anArrayOfStrings in: aDirectory
	| classOfOSSUnixSubprocess |
	classOfOSSUnixSubprocess := Smalltalk at: #OSSUnixSubprocess.
	classOfOSSUnixSubprocess new
		command: self gitCommand;
		arguments:
			{'-C'.
			(MCFileTreeFileUtils current directoryPathString: aDirectory)}
				, anArrayOfStrings;
		redirectStdout;
		redirectStderr;
		runAndWaitPollingEvery: (Delay forMilliseconds: 5)
			retrievingStreams: true
			onExitDo: [ :command :outString :errString | 
			command isSuccess
				ifTrue: [ ^ outString ]
				ifFalse: [ errString isEmpty
						ifTrue: [ ^ '' ]
						ifFalse: [ MCFileTreeGitError new signal: 'Git error: ' , errString ] ] ]