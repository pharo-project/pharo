transformations
notElimination
	^self
		rewrite: #(
			#('``@object not not'	"->"	'``@object') 
			#('``@object not ifTrue: ``@block' 	"->"	'``@object ifFalse: ``@block') 
			#('``@object not ifFalse: ``@block'	"->"	'``@object ifTrue: ``@block') 
			#('``@collection select: [:`each | | `@temps | ``@.Statements. ``@object not]'
				"->"	'``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object]')
			#('``@collection reject: [:`each | | `@temps | ``@.Statements. ``@object not]'
				"->"	'``@collection select: [:`each | | `@temps | ``@.Statements. ``@object]')
			#('[| `@temps | ``@.Statements. ``@object not] whileTrue: ``@block'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileFalse: ``@block')
			#('[| `@temps | ``@.Statements. ``@object not] whileFalse: ``@block'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileTrue: ``@block')
			#('[| `@temps | ``@.Statements. ``@object not] whileTrue'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileFalse')
			#('[| `@temps | ``@.Statements. ``@object not] whileFalse'
				"->"	'[| `@temps | ``@.Statements. ``@object] whileTrue')
			#('(``@a <= ``@b) not' "->" '``@a > ``@b')
			#('(``@a < ``@b) not' "->" '``@a >= ``@b')
			#('(``@a = ``@b) not' "->" '``@a ~= ``@b')
			#('(``@a == ``@b) not' "->" '``@a ~~ ``@b')
			#('(``@a ~= ``@b) not' "->" '``@a = ``@b')
			#('(``@a ~~ ``@b) not' "->" '``@a == ``@b')
			#('(``@a >= ``@b) not' "->" '``@a < ``@b')
			#('(``@a > ``@b) not' "->" '``@a <= ``@b'))			
		methods: false
		name: 'Eliminate unnecessary not''s'