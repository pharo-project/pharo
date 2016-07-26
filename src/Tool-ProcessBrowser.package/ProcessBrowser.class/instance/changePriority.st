process actions
changePriority
	| str newPriority nameAndRules |
	nameAndRules := self nameAndRulesForSelectedProcess.
	nameAndRules third
		ifFalse: [self inform: 'Nope, won''t change priority of ' , nameAndRules first.
			^ self].
	str := UIManager default 
				request: 'New priority' 
		  initialAnswer: selectedProcess priority asString.
	str ifNil: [str := String new].
	newPriority := str asNumber asInteger.
	newPriority
		ifNil: [^ self].
	(newPriority < 1
			or: [newPriority > Processor highestPriority])
		ifTrue: [self inform: 'Bad priority'.
			^ self].
	self class setProcess: selectedProcess toPriority: newPriority.
	self updateProcessList