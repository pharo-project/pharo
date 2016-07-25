*NautilusCommon
transformSelectorForComparison
	^ String
		streamContents: [ :stream | 
			self
				doWithIndex: [ :c :i | 
					(c isUppercase and: [ i > 1 ])
						ifTrue: [ stream << $: ].
					stream << c ] ]