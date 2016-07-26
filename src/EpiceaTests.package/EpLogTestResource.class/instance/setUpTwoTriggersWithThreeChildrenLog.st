running
setUpTwoTriggersWithThreeChildrenLog

	| builder triggerReference |
	builder := EpLogBuilder new.	
	builder logInitial.
	
	triggerReference := builder logHeadReference.

	builder
		logChildren: 1 
		triggerReference: triggerReference.

	builder
		logEvent: (EpLogEntriesComment entryReferences: { (EpClassAddition class: Integer) } oldComment: '' newComment: 'Passed Code Review')
		triggerReference: triggerReference.

	oneTriggerWithOneChildLog := builder log.