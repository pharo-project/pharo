widget method
buildTestSelectorFor: aMethod

	^ String streamContents: [:s || capitalize |
		capitalize := true.
		s << 'test'.
		aMethod selector do: [:c |
			c = $:
				ifTrue: [ capitalize := true ]
				ifFalse: [ capitalize
							ifTrue: [ 
								capitalize := false.
								s << c asUppercase. ]
							ifFalse:[ s << c ]]]]
			