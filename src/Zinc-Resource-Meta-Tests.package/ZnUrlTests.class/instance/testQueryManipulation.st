testing
testQueryManipulation
	| url |
	url := 'http://www.google.com/?one=1&two=2' asZnUrl.
	url queryAt: 'three' put: '3'.
	url queryRemoveKey: 'one'.
	self assert: url queryKeys sorted = #(three two).
	self assert: (url queryAt: 'two') = '2'.
	self assert: (url queryAt: 'three') = '3'.
	url queryRemoveAll.
	self deny: url hasQuery