private-events
exampleAdded: anAnnouncement
	anAnnouncement method gtExamples do: [ :example |
		self announcer announce: (GTExampleAdded new 
			example: example;
			yourself) ]