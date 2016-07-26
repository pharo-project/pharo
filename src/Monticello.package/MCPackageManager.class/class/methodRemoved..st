system changes
methodRemoved: anEvent
	self managersForClass: anEvent methodClass category: anEvent protocol do:[:mgr| mgr modified: true].
