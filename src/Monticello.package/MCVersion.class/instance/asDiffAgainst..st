converting
asDiffAgainst: aVersion
	aVersion info = self info ifTrue: [self error: 'Cannot diff against self!'].
	^ MCDiffyVersion
		package: self package
		info: self info
		snapshot: self snapshot
		dependencies: self dependencies
		baseVersion: aVersion