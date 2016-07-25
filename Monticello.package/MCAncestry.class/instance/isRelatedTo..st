ancestry
isRelatedTo: aVersionInfo
	^ aVersionInfo timeStamp < self timeStamp
		ifTrue: [self hasAncestor: aVersionInfo]
		ifFalse: [aVersionInfo hasAncestor: self]