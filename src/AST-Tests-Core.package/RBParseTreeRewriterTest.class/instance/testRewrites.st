tests - to be refined
testRewrites
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(
		(	'[:c | |a| a foo1; foo2]'	
			'[:c | |a| b foo1; foo2]'
			'a'
			'b' )
		(	'self foo: 1. bar foo1 foo: 2. (self foo: a) foo: (b foo: c)'
			'self bar: 1. bar foo1 bar: 2. (self bar: a) bar: (b bar: c)'
			'``@rcvr foo: ``@arg1'
			'``@rcvr bar: ``@arg1' )
		('3 + 4'	'4 + 4'		'3'		'4' )
		('a := self a'		'b := self a'		'a'		'b' )
		(	'^self at: 1 put: 2'
			'^self put: 1 put: 2'	
			'^`@rcvr `at: `@arg1 put: `@arg2'
			'^`@rcvr put: `@arg1 put: `@arg2' )
		('1 + 2 + 3'		'0 + 0 + 0'		'`#literal'		'0' )
		(
			'1 + 2 + 3. 3 foo: 4'
			'3 + (2 + 1). 4 foo: 3'
			'``@rcvr `msg: ``@arg'
			'``@arg `msg: ``@rcvr' )
		(	'self foo: a bar: b. 1 foo: a bar: b'
			'2 foo: a bar: b. 1 foo: a bar: b'
			'self `@msg: `@args'
			'2 `@msg: `@args' ) 
		(	'a := b. a := c + d'
			'b := a. a := c + d'
			'`var1 := `var2'
			'`var2 := `var1' )
		(	'^self foo value: 1'
			'self return: (self foo value: 1)'
			'^`@anything'
			'self return: `@anything' )
		(	'self first; second. self first; second. self a. self b'
			'2 timesRepeat: [self first; second]. self a. self b'
			'`.Stmt1. `.Stmt1. `@.stmts'
			'2 timesRepeat: [`.Stmt1]. `@.stmts' )
		(	'[:a | self a: 1 c: 2; b]'
			'[:a | self d: 2 e: 1; f. self halt]'
			'`@rcvr `msg1: `@arg1 `msg2: `@arg2; `msg'
			'`@rcvr d: `@arg2 e: `@arg1; f. self halt' )  ) do: 
		[:each | 
		| rewrite |
		rewrite := RBParseTreeRewriter new.
		rewrite replace: (each at: 3)
			with: each last.
		self compare: (RBParser parseExpression: (rewrite executeTree: (RBParser parseExpression: each first);
				 tree) formattedCode)
			to: (RBParser parseExpression: (each at: 2))]