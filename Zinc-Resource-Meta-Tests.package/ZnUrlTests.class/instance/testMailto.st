testing
testMailto
	| url |
	url := 'mailto:sven@beta9.be?subject=test' asZnUrl.
	self assert: url scheme equals: #mailto.
	self assert: url mailToAddress equals: 'sven@beta9.be'.
	self assert: (url queryAt: 'subject') equals: 'test'.
	self assert: url printString equals: 'mailto:sven@beta9.be?subject=test'