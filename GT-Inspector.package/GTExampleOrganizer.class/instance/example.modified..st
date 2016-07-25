private-events
example: oldExample modified: anAnnouncement
	anAnnouncement method gtExamples do: [ :example |
		self announcer announce: (GTExampleModified new
			oldExample: oldExample;
			example: example;
			yourself) ]