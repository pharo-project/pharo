accessing
sentMessages
	^ super sentMessages
		add: self selector;
		yourself