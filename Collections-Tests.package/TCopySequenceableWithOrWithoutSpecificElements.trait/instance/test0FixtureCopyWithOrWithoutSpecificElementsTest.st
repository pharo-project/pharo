tests - fixture
test0FixtureCopyWithOrWithoutSpecificElementsTest
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self indexInNonEmpty.
	self assert: self indexInNonEmpty > 0.
	self assert: self indexInNonEmpty <= self nonEmpty size