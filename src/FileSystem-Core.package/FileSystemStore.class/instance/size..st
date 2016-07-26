public
size: aPath
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicSize: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ]
	