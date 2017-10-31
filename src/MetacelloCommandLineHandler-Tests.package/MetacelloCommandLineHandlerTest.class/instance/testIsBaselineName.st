tests-testing
testIsBaselineName
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper').
	self assert: cli isBaselineName.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self deny: cli isBaselineName.