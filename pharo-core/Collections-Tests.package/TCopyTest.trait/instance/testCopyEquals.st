tests - copy
testCopyEquals
	"self debug: #testCopySameClass"
	"A copy should be equivalent to the things it's a copy of"
	
	| copy | 
	copy := self nonEmpty copy.
	self assert: copy  = self nonEmpty.