manifest
containsToDo: aItem onRule: ruleId version: versionId

	^  (self hasToDoOf: ruleId version: versionId) 
		and: [(self toDoOf: ruleId version: versionId) 
			anySatisfy: [ :fp| fp = aItem ]] 