manifest
containsRejectedRule: aRuleId
	
	^ self rejectRules anySatisfy: [ :fp | fp = aRuleId ]