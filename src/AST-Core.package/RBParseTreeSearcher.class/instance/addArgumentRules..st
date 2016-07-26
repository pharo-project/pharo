accessing
addArgumentRules: ruleCollection 
	ruleCollection do: [:each | self addArgumentRule: each]