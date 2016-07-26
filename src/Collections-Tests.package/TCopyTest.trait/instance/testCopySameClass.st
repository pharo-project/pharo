tests - copy
testCopySameClass
	"self debug: #testCopySameClass"
	"A copy of a collection should always be of the same class as the instance it copies"
	
	| copy | 
	copy := self empty copy.
	self assert: copy class == self empty class.