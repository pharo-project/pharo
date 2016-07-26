announcement handling
addEvent: anEvent 

	^ self 
		addEvent: anEvent
		triggerReference: self currentTriggerReference