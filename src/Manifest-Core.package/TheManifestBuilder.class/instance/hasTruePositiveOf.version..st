manifest
hasTruePositiveOf: ruleId version: versionId
	^ self manifest respondsTo: (self selectorTruePositiveOf: ruleId version: versionId)  
	