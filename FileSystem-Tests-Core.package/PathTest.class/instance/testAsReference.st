tests
testAsReference
	| path reference |
	path := Path * 'plonk'.
	reference := path asFileReference.
	self assert: reference class equals: FileReference.
	self assert: reference path equals: path