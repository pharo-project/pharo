testing
testComponentSpecifcEncoding
	"In a URL the safe set of percent encoding depends on where we are"
	
	| url |
	url := 'http://foo.com/bar@xx=2?yy=/#$1'.
	self assert: url asZnUrl printString equals: url