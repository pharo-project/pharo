tests - converting
testWithSeparatorsCompacted
	#( 
		'foo' 'foo'
		'foo bar' 'foo bar'
		'' ''
		' ' ' '
		'foo  bar  !' 'foo bar !'
		' foo  bar  ! ' ' foo bar ! '
		'  foo  bar  !  ' ' foo bar ! '
		'		tabs		tabs	!		' ' tabs tabs ! '
		'space followed by tab 	!' 'space followed by tab !'
	) pairsDo: [ :input :output |
		self assert: input withSeparatorsCompacted equals: output ]
	