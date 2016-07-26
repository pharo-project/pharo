process actions
suspendProcess
	| nameAndRules |
	selectedProcess isSuspended
		ifTrue: [^ self].
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules second
		ifFalse: [self inform: 'Nope, won''t suspend ' , nameAndRules first.
			^ self].
	self class suspendProcess: selectedProcess.
	self updateProcessList