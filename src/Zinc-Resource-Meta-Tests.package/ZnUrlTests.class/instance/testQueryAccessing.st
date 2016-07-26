testing
testQueryAccessing
	| url |
	url := 'http://www.google.com/?one=1&two=2' asZnUrl.
	self assert: url hasQuery.
	self assert: url queryKeys sorted equals: #(one two).
	self assert: (url queryAt: 'two' ifAbsent: [ self fail ]) equals: '2'.
	self assert: (url queryAt: 'three' ifAbsent: [ #missing ]) equals: #missing.
	url queryAt: 'one' ifPresent: [ :value | self assert: value equals: '1' ].
	self assert: (url queryAt: 'three' ifPresent: [ :value | self fail ]) isNil