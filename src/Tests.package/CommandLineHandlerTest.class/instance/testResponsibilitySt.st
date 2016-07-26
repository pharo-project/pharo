tests
testResponsibilitySt
	| args |
	args := self argumentsWith: #('/foo/bar/myScript.st').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self assert: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).
	
	args := self argumentsWith: #('st' '/foo/bar/myScript.st').
	self assert: (PharoCommandLineHandler isResponsibleFor: args).
	self assert: (STCommandLineHandler isResponsibleFor: args).
	self deny: (EvaluateCommandLineHandler isResponsibleFor: args).