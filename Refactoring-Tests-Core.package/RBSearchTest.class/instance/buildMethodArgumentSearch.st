accessing
buildMethodArgumentSearch
	"Two argument methods"
	self 
		createMethodSearchWith: '`arg1: `arg1 `arg2: `arg2 | `@temps | `@.Stmts'
		selectors: #(
				#subclassOf:overrides:
				#createMatcherFor:method:
				#createParseTreeRule:name:
			)
		inClass: RBBasicLintRuleTest class