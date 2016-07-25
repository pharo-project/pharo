public
createDirectory: path
	| parent |
	parent := path parent.
	^ self
		nodeAt: parent
		ifPresent: [ :entry | 
			entry
				fileEntryAt: path basename
				ifPresent: [ :node | 
					node isDirectory
						ifTrue: [ self signalDirectoryExists: path ]
						ifFalse: [ self signalFileExists: path ] ].
			entry ensureCreateDirectory: path basename  ]
		ifAbsent: [ self signalDirectoryDoesNotExist: parent ]