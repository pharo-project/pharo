tests - to be refined
testRewriteMethods
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(#('arg1: a arg2: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg1: a arg2: b' 'arg2: a arg1: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg2: b arg2: a' '`arg1: `var1 `arg2: `var2 | `@temps | ``@.stmts. self `arg1: `var1 `arg2: `var2. `@.stmts1' '`arg2: `var1 `arg1: `var2 | `@temps | ``@.stmts. self `arg2: `var2 `arg2: `var1. `@.stmts1') #('arg1: a arg2: b | temp1 temp2 | self stmt1. self arg1: a arg2: b' 'arg1: a arg2: b | temp1 temp2 | [self stmt1] repeat' '`@args: `@vars | `@temps | `@.stmts. self `@args: `@vars' '`@args: `@vars | `@temps | [`@.stmts] repeat') #('+ a | temps | ^self primitiveValue' '- a | temps | ^self primitiveValue' '+ `temp | `@tmps | `@.stmts' '- `temp | `@tmps | `@.stmts') #('a self stmt1. self stmt2' 'a self stmt1. self stmt2' 'b | `@temps | `@.stmts' 'c | `@temps | `@.stmts') #('a <foo: 1 bar: 2>' 'a <bar: 2 foo: 1>' 'a <`sel1: `#arg1 `sel2: `#arg2>' 'a <`sel2: `#arg2 `sel1: `#arg1>') #('a <foo> self foo' 'b <foo> self foo' 'a `@.stmts' 'b `@.stmts'))
		do: 
			[:each | 
			| rewrite |
			rewrite := RBParseTreeRewriter new.
			rewrite replaceMethod: (each at: 3) with: each last.
			self compare: (RBParser 
						parseMethod: (rewrite
								executeTree: (RBParser parseMethod: each first);
								tree) formattedCode)
				to: (RBParser parseMethod: (each at: 2)).
			rewrite := RBParseTreeRewriter new.
			rewrite replaceTree: (RBParser parseRewriteMethod: (each at: 3))
				withTree: (RBParser parseRewriteMethod: each last).
			self compare: (RBParser 
						parseMethod: (rewrite
								executeTree: (RBParser parseMethod: each first);
								tree) formattedCode)
				to: (RBParser parseMethod: (each at: 2))]