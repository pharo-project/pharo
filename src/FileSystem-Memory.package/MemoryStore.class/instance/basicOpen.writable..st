private
basicOpen: path writable: aBoolean
	^ self
		nodeAt: path
		ifPresent: [ :aMemoryFileSystemEntry | aMemoryFileSystemEntry ]
		ifAbsent: [ 
			aBoolean
				ifFalse: [ self signalFileDoesNotExist: path ]
				ifTrue: [ self createFile: path ] ]