samples
oneTriggerWithFourChildrenLog

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
		logChildren: 1
		triggerReference: triggerReference.

	^ self log