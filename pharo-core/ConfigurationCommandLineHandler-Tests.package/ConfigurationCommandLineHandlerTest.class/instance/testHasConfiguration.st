tests
testHasConfiguration
	| cli |
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20').
	self deny: cli hasConfiguration.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--install').
	self deny: cli hasConfiguration.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'ConfigurationOfFoo').
	self assert: cli hasConfiguration.