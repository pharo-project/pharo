accessing
closestAncestorVersionFor: anAncestry ifNone: errorBlock
	^ self
		cacheAllFileNamesDuring: [ 
			anAncestry breadthFirstAncestorsDo: [ :ancestorInfo | (self notCachedVersionWithInfo: ancestorInfo) ifNotNil: [ :v | ^ v ] ].
			^ errorBlock value ]