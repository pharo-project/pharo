public
isFile: aPath
	"slow solution for big directories! "
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsFile: entry ] 
		ifAbsent: [ ^ false ]
	