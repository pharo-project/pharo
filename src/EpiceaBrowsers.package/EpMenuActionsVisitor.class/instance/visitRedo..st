visitor
visitRedo: aRedoEvent
	^ (self visitEvent: aRedoEvent)
		add: self redoMenuAction;
		yourself.