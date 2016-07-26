adding/removing
removeRejectRule: ruleId 

	|  nfp |	
	nfp := self manifest rejectRules asOrderedCollection.
	nfp remove: ruleId ifAbsent: [^ self].
	nfp := nfp asArray.
	self compileSelector: (self class rejectRulesTag) returnValue:   nfp asString

	