image-management
bootstrapImageDo: arguments 
	
	OSSUnixSubprocess new
		command: 'pharo-vm/pharo';
		arguments: { '--nodisplay'. imageReference fullName }, arguments;
		redirectStdout;
		"redirectStderr"
		runAndWaitPollingEvery: 50 milliSeconds
			retrievingStreams: true
			onExitDo: [ :process :outString :errString  |
			process isSuccess
				ifTrue: [ ^ outString ]
				ifFalse: [ self error: outString , errString ] ]