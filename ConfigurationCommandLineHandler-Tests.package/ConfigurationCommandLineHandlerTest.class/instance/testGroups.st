tests
testGroups
	| cli |
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--group=foo').
	self assertCollection: cli groups equals: {'foo'}.
	
	cli := self command: #('http://ss3.gemstone.com/ss/MetaRepoForPharo20' '--group=foo,boo').
	self assertCollection: cli groups equals: {'foo' . 'boo'}.