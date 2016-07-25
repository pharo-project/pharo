transformations
unwindBlocks
	^self
		rewrite: #(
			#('[| `@temps | ``@.Statements. `var := ``@object] ensure: ``@block'
				"->"	'`var := [| `@temps | ``@.Statements. ``@object] ensure: ``@block') 
			#('[| `@temps | ``@.Statements. ^``@object] ensure: ``@block'
				"->"	'^[| `@temps | ``@.Statements. ``@object] ensure: ``@block') 
			#('[| `@temps | ``@.Statements. `var := ``@object] ifCurtailed: ``@block'
				"->"	'`var := [| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block') 
			#('[| `@temps | ``@.Statements. ^``@object] ifCurtailed: ``@block'
				"->"	'^[| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block'))
		methods: false
		name: 'Move assignment out of unwind blocks'