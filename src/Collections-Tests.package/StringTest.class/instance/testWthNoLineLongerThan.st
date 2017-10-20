tests - lines
testWthNoLineLongerThan
	| phrases |
	phrases := {
		'Fred the bear went down to the brook to read his book in silence'.
		'Fred the bear went down' , String cr , ' to the brook to ' , String cr , 'read his book in silence'.
		'Fred the bear went down' , String lf , '  to the brook to ' , String lf , 'read his book in silence'.
		'Fred the bear went down' , String crlf , '  to the brook to ' , String crlf , 'read his book in silence'.
		}.
	#(5 7 9) do: [:length |
		phrases do: [:phrase |
			| split |
			split := phrase withNoLineLongerThan: length.
			self
				assert: (split copyWithoutAll: CharacterSet separators) = (phrase copyWithoutAll: CharacterSet separators)
				description: 'no information should be lost'.
			split linesDo: [:line |
				self assert: line size <= length description: 'lines should not be longer than prescribed'].
			phrase lineCount = 1 ifTrue: [| lastLineSize |
				lastLineSize := length.
				split linesDo: [:line |
					line isEmpty ifFalse: [| firstWordSize |
						firstWordSize := line indexOfAnyOf: CharacterSet separators startingAt: 1 ifAbsent: [line size + 1].
						self assert: (lastLineSize + firstWordSize) > length description: 'split was too short'.
						lastLineSize := line size]]]]]