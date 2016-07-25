actions
actDueTo: anEventType
	self selected ifNil: [ ^ self ].
	self spotter
		historize: self selected;
		notifyActOn: self selected candidate for: anEventType.
	self selected processor actOn: self selected candidate for: self
