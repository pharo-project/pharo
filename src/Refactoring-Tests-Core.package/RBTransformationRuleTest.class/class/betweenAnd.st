transformations
betweenAnd
	^self rewrite: #(
			#('``@a >= ``@b and: [``@a <= ``@c]' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b & (``@a <= ``@c)' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a and: [``@a <= ``@c]' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a & (``@a <= ``@c)' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c and: [``@a >= ``@b]' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c & (``@a >= ``@b)' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a and: [``@a >= ``@b]' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a & (``@a >= ``@b)' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b and: [``@c >= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@a >= ``@b & (``@c >= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a and: [``@c >= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@b <= ``@a & (``@c >= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c and: [``@b <= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@a <= ``@c & (``@b <= ``@a)' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a and: [``@b <= ``@a]' "->" '``@a between: ``@b and: ``@c')
			#('``@c >= ``@a & (``@b <= ``@a)' "->" '``@a between: ``@b and: ``@c'))
		methods: false
		name: '"a >= b and: [a <= c]" -> "a between: b and: c"'