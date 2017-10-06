helpers
defaultRules
	^ self visibleRuleClasses
			reject: [ :ruleClass |
				"lint rules suck again…"
				((ruleClass inheritsFrom: ReAbstractRule) and: [ ruleClass enabled not ]) or: [ 
				self excludedRuleNames includes: ruleClass name ] ] 
			thenCollect: #new