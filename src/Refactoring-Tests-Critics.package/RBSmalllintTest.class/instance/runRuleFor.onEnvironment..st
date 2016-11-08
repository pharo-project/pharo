private
runRuleFor: aTestSelector onEnvironment: anEnvironment
	| rule |
	rule := (self ruleClassFor: aTestSelector) new.
	self assertIsValidRule: rule.
	RBSmalllintChecker
		runRule: rule
		onEnvironment: anEnvironment.
	^ rule.