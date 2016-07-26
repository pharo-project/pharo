options
optionCompileOnLinkInstallation: aBoolean
	aBoolean
		ifTrue: [ options add: #optionCompileOnLinkInstallation ] 
		ifFalse: [ options remove: #optionCompileOnLinkInstallation ifAbsent:[] ]