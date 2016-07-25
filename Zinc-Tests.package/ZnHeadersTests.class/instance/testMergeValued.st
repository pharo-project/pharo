testing
testMergeValued
	| headers |
	headers := ZnHeaders new.
	headers at: 'Set-Cookie' put: 'foo=1'.
	headers at: 'Set-Cookie' put: 'bar=1' ifPresentMerge: [ :old :new | old, ';', new ].
	self assert: (headers at: 'Set-Cookie') equals: 'foo=1;bar=1'
	
	