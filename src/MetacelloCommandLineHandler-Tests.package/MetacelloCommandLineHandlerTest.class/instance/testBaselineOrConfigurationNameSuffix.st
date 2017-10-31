tests-accessing
testBaselineOrConfigurationNameSuffix
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli baselineOrConfigurationNameSuffix equals: 'MineSweeper'.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli baselineOrConfigurationNameSuffix equals: 'Foo'.