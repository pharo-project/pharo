instance creation
named: aString on: aProcess startedAt: aContext
	^ self new
		name: aString;
		process: aProcess context: aContext;
		detectUIProcess