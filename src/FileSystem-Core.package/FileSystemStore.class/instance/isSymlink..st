public
isSymlink: aPath
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsSymlink: entry ] 
		ifAbsent: [ ^ false ].
	