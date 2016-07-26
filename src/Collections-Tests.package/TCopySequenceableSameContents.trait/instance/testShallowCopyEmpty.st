tests - copying same contents
testShallowCopyEmpty
	| result |
	result := self empty shallowCopy .
	self assert: result isEmpty .