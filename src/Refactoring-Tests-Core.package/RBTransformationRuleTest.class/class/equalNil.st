transformations
equalNil
	^self
		rewrite: #(
			#('``@object = nil'	"->"	'``@object isNil') 
			#('``@object == nil'	"->"	'``@object isNil') 
			#('``@object ~= nil'	"->"	'``@object notNil') 
			#('``@object ~~ nil'	"->"	'``@object notNil'))
		methods: false
		name: '= nil -> isNil AND ~= nil -> notNil'