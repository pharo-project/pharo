manifest
falsePositiveOf: ruleId version: versionId 

	^ self unwrap: (self manifest perform: (self selectorFalsePositiveOf: ruleId version: versionId))