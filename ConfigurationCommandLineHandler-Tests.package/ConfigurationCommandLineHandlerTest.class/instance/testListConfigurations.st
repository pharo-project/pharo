tests
testListConfigurations
	self skip: 'Currently quits the image when in non-interactive mode'.
	self activate: #('config' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20').