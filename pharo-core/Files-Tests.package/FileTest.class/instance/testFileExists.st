tests
testFileExists

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self assert: file exists.
	file delete.