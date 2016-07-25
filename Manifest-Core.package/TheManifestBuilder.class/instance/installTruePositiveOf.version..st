manifest
installTruePositiveOf: ruleId version: versionId
	
	self compileSelector: (self selectorTruePositiveOf: ruleId version: versionId) returnValue: '#()'
