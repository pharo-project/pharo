tests
testUnaryAccessingMethodWithoutReturn
	self
		ruleFor: self currentSelector
		plusSelectors:
			#(#unaryAccessingMessageStatementMethodWithoutReturn
				#unaryAccessingBranchingStatementMethodWithoutReturn)