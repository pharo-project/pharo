updating
changed: anAspect with: anObject

	self doAnnounce: (MorphChangedWithArguments new
		morph: self;
		selector: anAspect;
		arguments: anObject)