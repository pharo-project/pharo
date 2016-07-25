accessing
buildSimpleVariableSearch
	self 
		createSearchWith: 'result'
		selectors: #(
				#checkClass:
				#checkMethod:
				#isEmpty
				#problemCount
				#resetResult
				#resetResult
				#resetResult
				#result
				#result:
				#resultClass:
				#newResultClass:
				#viewResults
			)
		inClass: RBBasicLintRuleTest