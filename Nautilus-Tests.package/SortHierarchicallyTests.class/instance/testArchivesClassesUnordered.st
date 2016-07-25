tests
testArchivesClassesUnordered

	self
		nodes: (self sortByNameSize: archivesClasses)
		shouldBe: archivesClasses
		
	