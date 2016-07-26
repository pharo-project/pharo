tests
testUnrelatedClassesUnsorted

	self
		nodes: (self sortByNameSize: unrelatedClasses)
		shouldBe: unrelatedClasses