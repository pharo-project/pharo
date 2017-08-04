stdio
stdin

	^Stdin ifNil: [
		Stdin := self standardIOStreamNamed: #stdin forWrite: false.
		Stdin
			disableReadBuffering;
			yourself ].