testing
testMultiValued
	| headers |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' add: 'bar=1'.
	self assert: (headers at: 'Set-Cookie') equals: #('foo=1' 'bar=1')
	
	