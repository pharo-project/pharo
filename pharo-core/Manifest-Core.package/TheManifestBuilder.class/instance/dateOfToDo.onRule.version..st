manifest
dateOfToDo: aItem onRule: ruleId version: versionId
	
	 (self hasToDoOf: ruleId version: versionId)
		ifFalse: [^ self].
	^ self dateOf: ( aItem asRingDefinition) forSelector:  (self selectorToDoOf: ruleId version: versionId)
		
		