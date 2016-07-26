tests - copy
testCopyEmptyWithout
	"self debug: #testCopyEmptyWithout"
	| res |
	res := self empty copyWithout: self elementToAdd.
	self assert: res size = self empty size.
	self deny: (res includes: self elementToAdd)