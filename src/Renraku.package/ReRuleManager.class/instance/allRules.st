accessing
allRules

	^ rules ifNil: [
		rules := self class visibleRuleClasses
			reject: [ :ruleClass |
				self excludedRuleNames includes: ruleClass name ] 
			thenCollect: #new ]