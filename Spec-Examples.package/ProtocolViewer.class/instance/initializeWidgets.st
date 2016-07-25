initialization
initializeWidgets

	models := self instantiate: ModelList.
	protocols := self instantiate: ProtocolList.
	events := self instantiate: ProtocolList.
	
	protocols
		label: 'api';
		displayBlock: [ :m | m selector ].
	events
		label: 'api-events';
		displayBlock: [ :m | m selector ].
		
	self focusOrder 
		add: models;
		add: protocols;
		add: events
		