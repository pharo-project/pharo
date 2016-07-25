visitor
visitMonticelloVersionsLoad: aChange
	^ (self visitEvent: aChange)
		add: self redoMenuAction;
		yourself.