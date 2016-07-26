accessing
closestAncestorVersionFor: anAncestry ifNone: errorBlock
	^ anAncestry breadthFirstAncestors
		detect: [ :ea | self includesVersionWithInfo: ea ]
		ifFound: [ :info | self versionWithInfo: info ]
		ifNone: errorBlock