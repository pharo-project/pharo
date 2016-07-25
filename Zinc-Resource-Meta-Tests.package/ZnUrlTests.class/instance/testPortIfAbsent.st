testing
testPortIfAbsent
	self assert: ('scheme://host' asZnUrl portIfAbsent: 123) equals: 123.
	self assert: ('scheme://host' asZnUrl portIfAbsent: [ 123 ]) equals: 123.