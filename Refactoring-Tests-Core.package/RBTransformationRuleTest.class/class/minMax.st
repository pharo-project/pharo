transformations
minMax
	^self rewrite: #(
			#('``@a < ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a min: ``@b')
			#('``@a <= ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a min: ``@b')
			#('``@a > ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a max: ``@b')
			#('``@a >= ``@b ifTrue: [``@a] ifFalse: [``@b]'	"->"	'``@a max: ``@b')
			#('``@a < ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a max: ``@b')
			#('``@a <= ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a max: ``@b')
			#('``@a > ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a min: ``@b')
			#('``@a >= ``@b ifTrue: [``@b] ifFalse: [``@a]'	"->"	'``@a min: ``@b')
			#('`a < ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a <= ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a < ``@b ifFalse: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a <= ``@b ifFalse: [`a := ``@b]'			"->"	'`a := `a min: ``@b')
			#('`a > ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a >= ``@b ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('`a > ``@b ifFalse: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('`a >= ``@b ifFalse: [`a := ``@b]'			"->"	'`a := `a max: ``@b')
			#('``@b < `a ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b <= `a ifTrue: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b < `a ifFalse: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b <= `a ifFalse: [`a := ``@b]'			"->"	'`a := `a max: ``@b')
			#('``@b > `a ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b >= `a ifTrue: [`a := ``@b]'				"->"	'`a := `a max: ``@b')
			#('``@b > `a ifFalse: [`a := ``@b]'				"->"	'`a := `a min: ``@b')
			#('``@b >= `a ifFalse: [`a := ``@b]'			"->"	'`a := `a min: ``@b'))
		methods: false
		name: 'Rewrite ifTrue:ifFalse: using min:/max:'