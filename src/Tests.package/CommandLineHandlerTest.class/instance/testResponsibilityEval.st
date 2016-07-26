tests
testResponsibilityEval
	| args |
	args := self argumentsWith: #('eval' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('-e' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('--evaluate' '1+2').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self deny: (STCommandLineHandler isResponsibleFor: args).
	self assert: (EvaluateCommandLineHandler isResponsibleFor: args).