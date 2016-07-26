manifest
containsFalsePositive: aItem onRule: ruleId version: versionId

	^  (self hasFalsePositiveOf: ruleId version: versionId) 
			and: [ (self falsePositiveOf: ruleId version: versionId) anySatisfy: [ :fp | fp = aItem ]] 