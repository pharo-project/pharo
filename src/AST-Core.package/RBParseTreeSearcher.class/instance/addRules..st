accessing
addRules: ruleCollection 
	ruleCollection do: [:each | self addRule: each]