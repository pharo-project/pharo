accessing
workingDirectoryName
	^ self isBare
			ifTrue: [ nil ]
			ifFalse: [ self workingDirectory basename ]