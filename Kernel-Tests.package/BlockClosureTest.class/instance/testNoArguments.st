tests
testNoArguments
	[10
		timesRepeat: [:arg | 1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 1 arguments.'].
	[10
		timesRepeat: [:arg1 :arg2 | 1 + 2]]
		ifError: [:err  | self deny: err = 'This block requires 2 arguments.'] 