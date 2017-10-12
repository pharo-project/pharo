tests - converting
testWithoutLeadingDigits
	#( 
		'foo' 'foo'
		'123foo' 'foo'
		'123 bar7' 'bar7'
		' 123 	bar7' 'bar7'
		'foo.123.txt' 'foo.123.txt'
		'' ''
		'1' ''
		'1 2 3 ' ''
	) pairsDo: [ :input :output |
		self assert: input withoutLeadingDigits equals: output ]
	