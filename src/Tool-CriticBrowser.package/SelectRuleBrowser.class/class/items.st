instance creation
items

	| rules rulesByGroup integrationRules |
	rules := (ReRuleManager visibleRuleClasses collect: #new) sorted: [ :a :b |
		a name < b name ].
	integrationRules := rules select: [ :r |
		PharoCriticRules pharoHardLintRules includes: r class ].
	rulesByGroup := ((rules groupedBy: #group) associations collect: [ :as |
		CriticBrowserRulesGroup named: as key rules: as value ]).
	rulesByGroup sort: [ :a :b | a name < b name ].
	^ rulesByGroup copyWithFirst: (
		CriticBrowserRulesGroup
			named: 'Pharo integration rules'
			rules: integrationRules)