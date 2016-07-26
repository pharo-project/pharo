transformations
showWhileBlocks
	^self
		rewrite: #(
			#('``@cursor showWhile: [| `@temps | ``@.Statements. `var := ``@object]'
				"->"	'`var := ``@cursor showWhile: [| `@temps | ``@.Statements. ``@object]') 
			#('``@cursor showWhile: [| `@temps | ``@.Statements. ^``@object]'
				"->"	'^``@cursor showWhile: [| `@temps | ``@.Statements. ``@object]'))
		methods: false
		name: 'Move assignment out of showWhile: blocks'