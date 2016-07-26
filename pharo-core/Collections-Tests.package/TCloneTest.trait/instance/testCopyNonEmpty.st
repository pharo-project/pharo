tests - copy - clone
testCopyNonEmpty
	"self debug: #testCopyNonEmpty"
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: copy isEmpty.
	self assert: copy size = self nonEmpty size.
	self nonEmpty do: 
		[:each | copy includes: each]