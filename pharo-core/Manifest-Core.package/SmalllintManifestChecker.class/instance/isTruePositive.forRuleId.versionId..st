manifest
isTruePositive: aCritic forRuleId: ruleId versionId: versionId 
	
	^ (self manifestBuilderOf: aCritic)
			containsTruePositive: aCritic onRule:  ruleId version: versionId
