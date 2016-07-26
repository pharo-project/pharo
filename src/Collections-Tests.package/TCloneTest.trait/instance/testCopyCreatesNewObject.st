tests - copy - clone
testCopyCreatesNewObject
	"self debug: #testCopyCreatesNewObject"
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
	