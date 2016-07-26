transformations
detectIfNone
	^self rewrite: #(
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) isNil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) = nil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) == nil'
				"->"	'(``@collection contains: [:`each | | `@temps | ``@.Statements]) not')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) notNil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~= nil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]')
			#('(``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [nil]) ~~ nil'
				"->"	'``@collection contains: [:`each | | `@temps | ``@.Statements]'))
		methods: false
		name: 'detect:ifNone: -> contains:'