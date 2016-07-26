accessing
buildStatementSearch
	"Two or more statements"
	self 
		createSearchWith: '| `@temps | ``@.Stmts1. ``.Stmt1. ``@.Stmts2. ``.Stmt2. ``@.Stmts3'
		selectors: #(
				#checkMethod:
				#rewriteUsing:
				#viewResults
				#superSends
			)
		inClass: RBTransformationRuleTest