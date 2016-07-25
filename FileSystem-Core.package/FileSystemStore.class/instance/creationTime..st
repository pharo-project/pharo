public
creationTime: aPath
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicCreationTime: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	