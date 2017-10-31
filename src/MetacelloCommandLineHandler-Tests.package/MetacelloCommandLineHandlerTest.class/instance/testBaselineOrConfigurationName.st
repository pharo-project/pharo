tests-accessing
testBaselineOrConfigurationName
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli baselineOrConfigurationName equals: 'BaselineOfMineSweeper'.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli baselineOrConfigurationName equals: 'ConfigurationOfFoo'.