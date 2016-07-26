tests - copy
testCopyReturnsIdentity
	"self debug: #testCopyReturnsIdentity"
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: self nonEmpty == copy.
	