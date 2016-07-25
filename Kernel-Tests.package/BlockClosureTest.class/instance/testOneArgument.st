tests
testOneArgument
	| c |
	c := OrderedCollection new.
	c add: 'hello'.
	[c
		do: [1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 0 arguments.'].
	[c
		do: [:arg1 :arg2 | 1 + 2]]
		ifError: [:err | self deny: err = 'This block requires 2 arguments.'] 