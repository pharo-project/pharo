manifest
installFalsePositiveOf: ruleId version: versionId

	self compileSelector: (self selectorFalsePositiveOf: ruleId version: versionId) returnValue: '#()'
