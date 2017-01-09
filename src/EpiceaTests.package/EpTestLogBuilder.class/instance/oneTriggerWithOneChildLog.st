samples
oneTriggerWithOneChildLog

	| triggerReference |
	self logInitial.
	
	triggerReference := self logHeadReference.

	self
		logChildren: 2 
		triggerReference: triggerReference.

	self
		logEvent: (EpClassAddition class: Integer)
		triggerReference: triggerReference.
		
	self
		logEvent: (EpLogEntriesComment entryReferences: { self logHeadReference } oldComment: '' newComment: 'Passed Code Review')
		triggerReference: triggerReference.

	^ self log