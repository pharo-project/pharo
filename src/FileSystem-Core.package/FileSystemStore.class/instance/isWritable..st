public
isWritable: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsWritable: entry ] 
		ifAbsent: [ ^ false ].
	