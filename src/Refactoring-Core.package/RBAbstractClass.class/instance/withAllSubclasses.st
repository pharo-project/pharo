accessing
withAllSubclasses
	^(self allSubclasses)
		add: self;
		yourself