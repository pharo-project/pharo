tests-activation
testCheckInputForInstallSubcommand
	| cli |
	cli := self command: #('install' 'github://juliendelplanque/MineSweeper/repository' 'BaselineOfMineSweeper' '--version=master').
	self should: [ cli checkInputForInstallSubcommand ] raise: Exit.
	
	cli := self command: #('install' 'http://ss3.gemstone.com/ss/MetaRepoForPharo20' 'Foo').
	self should: [ cli checkInputForInstallSubcommand ] raise: Exit.