defaults
defaultDirectory
	| directories |
	directories := {
		((FileLocator localDirectory / 'package-cache') asFileReference).	
		(FileLocator temp).	
		(FileLocator home)}.
	directories
		do: [ :directory | 
			[ ^ directory ensureCreateDirectory; yourself ]
				on: Error do: [ "ignore and try the next directory" ] ].
	Error signal: 'Can''t create a package-cache'