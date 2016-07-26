tests - copy - clone
testCopyEmpty
	"self debug: #testCopyEmpty"
	
	| copy | 
	copy := self empty copy.
	self assert: copy isEmpty.