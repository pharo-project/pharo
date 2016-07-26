manifest
dateOfTruePositive: aItem onRule: ruleId version: versionId

	(self hasTruePositiveOf: ruleId version: versionId)
		ifFalse: [^ self].
	^ self dateOf: ( aItem asRingDefinition) forSelector:  (self selectorTruePositiveOf: ruleId version: versionId)
		
		