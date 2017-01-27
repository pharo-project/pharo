public
creationTimeOf: aPath
	"Return the date of creation of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicCreationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	