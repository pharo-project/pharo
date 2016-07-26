accessing
version
	^ version
		ifNil: [ 
			Cursor wait
				showWhile: [ version := repository versionFromFileNamed: (versions detect: [ :e | e last name = selectedVersion ]) last ].
			version ]