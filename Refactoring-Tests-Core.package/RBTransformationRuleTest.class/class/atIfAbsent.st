transformations
atIfAbsent
	^self rewrite: #(
			#('``@dictionary at: ``@key 
					ifAbsent: [| `@temps | 
							``@.Statements1.
							``@dictionary at: ``@key put: ``@object.
							``@.Statements2.
							``@object]'
			"->"
			'``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements1.
							``@.Statements2.
							``@object]')
			#('``@dictionary at: ``@key
					ifAbsent: [| `@temps |
							``@.Statements.
							``@dictionary at: ``@key put: ``@object]'
			"->"
			'``@dictionary at: ``@key
					ifAbsentPut: [| `@temps |
							``@.Statements.
							``@object]'))
		methods: false
		name: 'at:ifAbsent: -> at:ifAbsentPut:'