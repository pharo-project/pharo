process actions
terminateProcess
	| nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules second
		ifFalse: [self inform: 'Nope, won''t kill ' , nameAndRules first.
			^ self].
	self class terminateProcess: selectedProcess.	
	self updateProcessList