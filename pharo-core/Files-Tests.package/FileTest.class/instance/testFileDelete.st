tests
testFileDelete

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	file delete.
	self deny: file exists.