private
runRuleFor: aTestSelector onEnvironment: anEnvironment
	| rule runner |
	rule := (self ruleClassFor: aTestSelector) new.
	self assertIsValidRule: rule.
	runner := ReSmalllintChecker new.
	runner 
		rule: {rule};
		environment: anEnvironment;
		run.
	^ runner criticsOf: rule.