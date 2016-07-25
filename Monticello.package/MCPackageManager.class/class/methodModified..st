system changes
methodModified: anEvent
	^self managersForClass: anEvent methodClass selector: anEvent selector do:[:mgr| mgr modified: true].