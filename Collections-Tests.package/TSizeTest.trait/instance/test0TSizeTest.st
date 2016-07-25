tests - fixture
test0TSizeTest
	self empty.
	self sizeCollection.
	self assert: self empty isEmpty.
	self deny: self sizeCollection isEmpty