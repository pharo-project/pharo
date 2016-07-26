manifest
hasToDoOf: ruleId version: versionId
	^ self manifest respondsTo: (self selectorToDoOf: ruleId version: versionId)  
	