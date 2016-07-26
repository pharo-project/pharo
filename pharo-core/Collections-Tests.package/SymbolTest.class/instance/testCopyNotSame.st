tests - copy
testCopyNotSame
	"There is only one symbol, the copy is the same"
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: copy  == self nonEmpty.