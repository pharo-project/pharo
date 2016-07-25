manifest
hasFalsePositiveOf: ruleId version: versionId
	^ self manifest respondsTo: (self selectorFalsePositiveOf: ruleId version: versionId)