tests - converting
testWithoutTrailingDigits
	#( 
		'foo' 'foo'
		'foo123' 'foo'
		'bar 123 ' 'bar'
		'foo.123.txt' 'foo.123.txt'
		'' ''
		'1' ''
		'1 2 3 ' ''
	) pairsDo: [ :input :output |
		self assert: input withoutTrailingDigits equals: output ]
	