manifest
truePositiveOf: ruleId version: versionId 

	^ self unwrap: (self manifest perform: (self selectorTruePositiveOf: ruleId version: versionId))