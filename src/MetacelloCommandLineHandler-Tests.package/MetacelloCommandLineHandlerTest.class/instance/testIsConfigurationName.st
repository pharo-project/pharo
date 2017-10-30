tests-testing
testIsConfigurationName
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self deny: cli isConfigurationName.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli isConfigurationName.