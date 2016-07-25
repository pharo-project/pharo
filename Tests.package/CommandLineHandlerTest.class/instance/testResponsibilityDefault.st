tests
testResponsibilityDefault
	| args |
	args := self argumentsWith: #('--help').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).