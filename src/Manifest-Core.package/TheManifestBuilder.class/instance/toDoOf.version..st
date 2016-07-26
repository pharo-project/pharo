manifest
toDoOf: ruleId version: versionId 

	^ self unwrap: (self manifest perform: (self selectorToDoOf: ruleId version: versionId))