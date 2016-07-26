private-commands
runOSProcessGitCommand: anArrayOfStrings in: aDirectory
	| classOfExternalPipe classOfPipeableOSProcess c r d in out err |
	classOfExternalPipe := Smalltalk at: #ExternalPipe.
	classOfPipeableOSProcess := Smalltalk at: #PipeableOSProcess.
	in := classOfExternalPipe nonBlockingPipe.
	out := classOfExternalPipe nonBlockingPipe.
	err := classOfExternalPipe nonBlockingPipe.
	[ c := classOfPipeableOSProcess externalProxyClass
		programName: self gitCommand
		arguments: anArrayOfStrings
		initialEnvironment: nil.
	c initialStdIn: in reader.
	c initialStdOut: out writer.
	c initialStdErr: err writer.
	c pwd: (MCFileTreeFileUtils current directoryPathString: aDirectory).
	c := c initialize value.
	d := nil.
	r := out upToEnd.
	[ c update: #runState.
	c isComplete ]
		whileFalse: [ (d ifNil: [ d := Delay forMilliseconds: 5 ]) wait.
			r := r , out upToEnd ].
	r := r , out upToEndOfFile.
	c succeeded
		ifFalse: [ | errorString |
			errorString := err upToEndOfFile.
			errorString notEmpty
				ifTrue: [ MCFileTreeGitError new signal: 'Git error: ' , errorString ].
			r := '' ] ]
		ensure: [ in close.
			out close.
			err close ].
	^ r