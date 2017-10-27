tests - converting
testWithoutPeriodSuffix
	#( 
		'foo' 'foo'
		'foo.' 'foo'
		'foo.bar' 'foo'
		'foo.bar.txt' 'foo'
		'' ''
		'.' '.'
	) pairsDo: [ :input :output |
		self assert: input withoutPeriodSuffix equals: output ]
	