accessing
closestAncestorVersionFor: anAncestry ifNone: errorBlock
	anAncestry breadthFirstAncestorsDo:
		[:ancestorInfo |
		(self versionWithInfo: ancestorInfo) ifNotNil: [:v | ^ v]].
	^ errorBlock value