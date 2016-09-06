tests
testNormalizedSelectorForComparison
	#(#one #one: #two #two: #one:two: #oneTwo: #+ #- #+-)
		with: #('one' 'one' 'two' 'two' 'onetwo' 'onetwo' '+' '-' '+-')
		do: [ :original :transformed | 
			self
				assert: original normalizedSelectorForComparison = transformed;
				assert: original asString normalizedSelectorForComparison = transformed;
				assert: transformed normalizedSelectorForComparison = transformed.

			"should always return a string copy"
			self
				deny: original normalizedSelectorForComparison == original;
				deny: transformed normalizedSelectorForComparison == transformed;
				deny: original normalizedSelectorForComparison isSymbol;
				deny: transformed normalizedSelectorForComparison isSymbol ]