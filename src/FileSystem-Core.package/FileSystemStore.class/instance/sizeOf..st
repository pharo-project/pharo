public
sizeOf: aPath
	"Return the size of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicSizeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ]
	