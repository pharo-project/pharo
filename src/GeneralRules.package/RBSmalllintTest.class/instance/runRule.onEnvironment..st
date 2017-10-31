private
runRule: aRule onEnvironment: anEnvironment
	| runner |
	self assertIsValidRule: aRule.
	runner := ReSmalllintChecker new.
	runner 
		rule: {aRule};
		environment: anEnvironment;
		run.
	^ runner.