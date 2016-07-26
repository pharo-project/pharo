accessing
sentMessages
	^ super sentMessages
		remove: self selector ifAbsent: [ ];
		yourself