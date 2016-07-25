tests
testEqual
	| other |
	other := self createFileSystem.
	self assert: filesystem = other