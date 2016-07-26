tests - copy
testCopyNotSame
	"self debug: #testCopySameClass"
	"A copy of a collection should always be of the same class as the instance it copies"
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: copy  == self nonEmpty.