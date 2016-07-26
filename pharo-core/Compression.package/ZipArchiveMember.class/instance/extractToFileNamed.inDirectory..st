accessing
extractToFileNamed: aLocalFileName inDirectory: dir
	| file fullDir |
	self isEncrypted
		ifTrue: [ ^self error: 'encryption unsupported' ].
	file := dir / aLocalFileName.
	file parent ensureCreateDirectory.
	self isDirectory 
		ifFalse: [
			fullDir 
				forceNewFileNamed: file basename 
				do: [:stream |  self extractTo: stream]]
		ifTrue: [ fullDir ensureCreateDirectory ]
