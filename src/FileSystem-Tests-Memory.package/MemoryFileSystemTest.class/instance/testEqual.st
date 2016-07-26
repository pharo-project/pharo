tests
testEqual
	| other |
	other := self createFileSystem.
	self deny: filesystem = other