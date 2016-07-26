running
setUpOneTriggerWithOneChildLog

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
		logEvent: (EpLogEntriesComment entryReferences: { builder logHeadReference } oldComment: '' newComment: 'Passed Code Review')
		triggerReference: triggerReference.

	oneTriggerWithOneChildLog := builder log.