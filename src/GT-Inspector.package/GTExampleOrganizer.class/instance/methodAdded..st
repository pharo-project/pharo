private-events
methodAdded: anAnnouncement
	self withoutExceptionsDo: [
		"self exampleAdded: anAnnouncement."
		self checkForMissingDependencies: anAnnouncement method.
		self checkForNumberOfArguments: anAnnouncement method.
		self checkForMissingClasses: anAnnouncement method.
		GTExampleNautilus methodChanged: anAnnouncement method ]