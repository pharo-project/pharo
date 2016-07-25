running
setUpOneTriggerWithFourChildrenLog

	| builder triggerReference |
	builder := EpLogBuilder new.	
	builder logInitial.
	
	triggerReference := builder logHeadReference.

	builder
		logChildren: 2 
		triggerReference: triggerReference.

	builder
		logEvent: (EpClassAddition class: Integer)
		triggerReference: triggerReference.

	builder
		logChildren: 1
		triggerReference: triggerReference.

	oneTriggerWithFourChildrenLog := builder log.