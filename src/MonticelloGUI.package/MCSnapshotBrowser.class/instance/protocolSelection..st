selecting
protocolSelection: anInteger
	protocolSelection := (anInteger = 0 ifFalse: [self visibleProtocols at: anInteger]).
	self methodSelection: 0.
	self changed: #protocolSelection;
		changed: #methodList.