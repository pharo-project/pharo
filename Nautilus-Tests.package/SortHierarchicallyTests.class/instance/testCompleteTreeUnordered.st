tests
testCompleteTreeUnordered

	self
		nodes: (self sortByNameSize: completeTree)
		shouldBe: completeTree