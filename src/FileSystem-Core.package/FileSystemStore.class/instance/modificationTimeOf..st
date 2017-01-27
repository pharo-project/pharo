public
modificationTimeOf: aPath
	"Returns the last date of modification of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicModificationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	