tests
testResponsibility
	| args |
	args := self argumentsWith: #('config').
	self assert: (ConfigurationCommandLineHandler isResponsibleFor: args).