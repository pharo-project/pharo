accessing
stdout
	(stdout isNil or: [ stdout closed ])
		ifTrue: [ stdout := self createStdout ].
		
	^ stdout