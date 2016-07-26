manifest
installToDoOf: ruleId version: versionId

	self compileSelector: (self selectorToDoOf: ruleId version: versionId) returnValue: '#()'
