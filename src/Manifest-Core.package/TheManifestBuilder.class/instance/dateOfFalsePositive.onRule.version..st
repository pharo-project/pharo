manifest
dateOfFalsePositive: aItem onRule: ruleId version: versionId

	(self hasFalsePositiveOf: ruleId version: versionId)
		ifFalse: [^ self].
		
	^ self dateOf: ( aItem asRingDefinition) forSelector:  (self selectorFalsePositiveOf: ruleId version: versionId)
		
		