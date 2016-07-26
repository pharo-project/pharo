adding/removing
addRejectRule: ruleId 

	|  nfp |	
	nfp := self manifest rejectRules asSet.
	nfp add: ruleId.
	nfp := nfp asArray.
	self compileSelector: (self class rejectRulesTag) returnValue: nfp asString
