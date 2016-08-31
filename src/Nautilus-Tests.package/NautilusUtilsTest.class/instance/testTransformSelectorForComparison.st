tests
testTransformSelectorForComparison
	#(one one: two two: one:two: oneTwo: #'+' #'-' #'+-')
		with: #('one' 'one' 'two' 'two' 'onetwo' 'onetwo' '+' '-' '+-')
		do: [:original :transformed |
			self
				assert: original transformSelectorForComparison = transformed;
				assert: original asString transformSelectorForComparison = transformed;
				assert: transformed transformSelectorForComparison = transformed.

			"should always return a string copy"
			self
				deny: original transformSelectorForComparison == original;
				deny: transformed transformSelectorForComparison == transformed;
				deny: original transformSelectorForComparison isSymbol;
				deny: transformed transformSelectorForComparison isSymbol]