manifest
containsTruePositive: aItem onRule: ruleId version: versionId

	^  (self hasTruePositiveOf: ruleId version: versionId) 
		and: [(self truePositiveOf: ruleId version: versionId) anySatisfy: [ :fp| fp = aItem ]] 