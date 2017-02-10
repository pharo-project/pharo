private
addEvent: anEvent newEntryDo: aBlock

	self 
		addEvent: anEvent
		newEntryDo: aBlock
		triggerReference: self currentTriggerReference