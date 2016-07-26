testing
ifFile: fBlock ifDirectory: dBlock ifAbsent: aBlock
	^ self isFile
		ifTrue: fBlock
		ifFalse: [
			self isDirectory
				ifTrue: dBlock
				ifFalse: aBlock ]